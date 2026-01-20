;;;; engine.lisp - XMPP engine for CLabber using cl-xmpp

(in-package #:clabber)

;;; Debug logging to file
(defvar *debug-log-stream* nil)
(defvar *debug-log-path* "/tmp/clabber-debug.log")

(defun debug-log (format-string &rest args)
  "Log a debug message to the debug log file."
  (when *debug-log-stream*
    (apply #'format *debug-log-stream* format-string args)
    (terpri *debug-log-stream*)
    (force-output *debug-log-stream*)))

(defun open-debug-log ()
  "Open the debug log file."
  (setf *debug-log-stream* 
        (open *debug-log-path* :direction :output 
              :if-exists :supersede :if-does-not-exist :create)))

(defun close-debug-log ()
  "Close the debug log file."
  (when *debug-log-stream*
    (close *debug-log-stream*)
    (setf *debug-log-stream* nil)))

(defclass xmpp-engine ()
  ((queue      :initform (make-event-queue) :reader engine-queue)
   (thread     :initform nil :accessor engine-thread)
   (running    :initform nil :accessor engine-running-p)
   (connection :initform nil :accessor engine-connection)
   (jid        :initform nil :accessor engine-jid)
   (resource   :initform "CLabber" :accessor engine-resource))
  (:documentation "XMPP engine using cl-xmpp library."))

(defun parse-jid (jid)
  "Parse JID into (values user domain resource)."
  (let* ((at-pos (position #\@ jid))
         (slash-pos (position #\/ jid)))
    (values (if at-pos (subseq jid 0 at-pos) jid)
            (if at-pos
                (subseq jid (1+ at-pos) (or slash-pos (length jid)))
                nil)
            (if slash-pos (subseq jid (1+ slash-pos)) nil))))

(defun engine-start (engine jid password &key host port mucs)
  "Start the XMPP engine and connect."
  (setf (engine-running-p engine) t)
  (setf (engine-jid engine) jid)
  (multiple-value-bind (user domain resource) (parse-jid jid)
    (when resource
      (setf (engine-resource engine) resource))
    (let ((eng engine)
          (usr user)
          (dom domain)
          (pwd password)
          (hst host)
          (prt (or port 5222))
          (res (engine-resource engine))
          (muc-rooms mucs))
      (setf (engine-thread engine)
            (bt:make-thread
             (lambda ()
               ;; Enable debug logging to file
               (open-debug-log)
               (setf xmpp:*debug-stream* *debug-log-stream*)
               (debug-log "=== CLabber XMPP Engine Starting ===")
               (handler-case
                   (let* ((hostname (or hst dom))
                          (conn (xmpp:connect-tls :hostname hostname
                                                  :port prt
                                                  :jid-domain-part dom)))
                     (setf (engine-connection eng) conn)
                     (q-push (engine-queue eng)
                             (make-instance 'xmpp-connecting :jid jid))
                       ;; Authenticate using SASL PLAIN
                       (xmpp:auth conn usr pwd res :mechanism :sasl-plain)
                       ;; If we get here, auth succeeded
                       (q-push (engine-queue eng)
                               (make-instance 'xmpp-connected :jid jid))
                       ;; Request roster
                       (xmpp:get-roster conn)
                       ;; Request bookmarks (XEP-0048) for MUC rooms
                       (debug-log "Requesting bookmarks...")
                       (engine-request-bookmarks conn)
                       ;; Auto-join MUC rooms from config
                       (when muc-rooms
                         (debug-log "Auto-joining ~d MUC rooms from config" (length muc-rooms))
                         (dolist (room muc-rooms)
                           (debug-log "Joining MUC: ~a" room)
                           (engine-join-muc-raw conn room usr))
                         ;; Add all MUC rooms to roster at once
                         (q-push (engine-queue eng)
                                 (make-instance 'bookmarks-update
                                                :rooms (mapcar (lambda (room)
                                                                 (list :jid room :name room :autojoin t))
                                                               muc-rooms))))
                       ;; Enter receive loop
                       (engine-receive-loop eng))
                   (error (e)
                     (q-push (engine-queue eng)
                             (make-instance 'error-event
                                            :where :xmpp
                                            :condition (format nil "~a" e)))
                     (q-push (engine-queue eng)
                             (make-instance 'xmpp-disconnected)))))
             :name "clabber-xmpp-thread"))))
  engine)

(defun engine-receive-loop (engine)
  "Main receive loop for XMPP stanzas."
  (let ((conn (engine-connection engine))
        (stanza-count 0))
    (debug-log "Entering receive loop...")
    (loop while (and (engine-running-p engine)
                     conn
                     (xmpp:connectedp conn))
          do (handler-case
                 (let ((stanza (xmpp:receive-stanza conn)))
                   (when stanza
                     (incf stanza-count)
                     (debug-log "Stanza #~d: ~a" stanza-count (type-of stanza))
                     (engine-handle-stanza engine stanza)))
               (error (e)
                 (debug-log "Receive error: ~a" e)))))
  ;; Connection ended
  (q-push (engine-queue engine)
          (make-instance 'xmpp-disconnected)))

(defun engine-handle-stanza (engine stanza)
  "Handle a received XMPP stanza and convert to events."
  (handler-case
      (typecase stanza
        ;; Message stanza
        (xmpp:message
         (let ((from (xmpp:from stanza))
               (body (xmpp:body stanza)))
           (when (and from body (> (length body) 0))
             (q-push (engine-queue engine)
                     (make-instance 'xmpp-message :from from :body body)))))
        
        ;; Presence stanza
        (xmpp:presence
         (let ((from (xmpp:from stanza)))
           (when from
             (let ((type- (ignore-errors (slot-value stanza 'xmpp::type-)))
                   (show (ignore-errors (slot-value stanza 'xmpp::show))))
               (q-push (engine-queue engine)
                       (make-instance 'xmpp-presence
                                      :jid from
                                      :show (or show
                                                (if (and type- (string= type- "unavailable"))
                                                    "offline"
                                                    "available"))))))))
        
        ;; Roster response
        (xmpp:roster
         (let ((items (mapcar (lambda (item)
                                (make-instance 'roster-item
                                               :jid (xmpp:jid item)
                                               :name (ignore-errors (slot-value item 'xmpp::name))))
                              (xmpp:items stanza))))
           (q-push (engine-queue engine)
                   (make-instance 'roster-update :items items))))
        
        ;; xml-element: IQ responses come back as this type
        (xmpp::xml-element
         (let* ((name (ignore-errors (xmpp::name stanza)))
                (xml-str (ignore-errors
                           (with-output-to-string (s)
                             (format s "IQ name=~a, elements=~a" 
                                     name
                                     (length (ignore-errors (xmpp::elements stanza))))))))
           (q-push (engine-queue engine)
                   (make-instance 'error-event
                                  :where :iq
                                  :condition (or xml-str "xml-element received")))
           ;; Check for bookmarks in IQ result
           (when (eq name :iq)
             (let ((bookmarks (engine-extract-bookmarks-from-xml stanza)))
               (when bookmarks
                 (q-push (engine-queue engine)
                         (make-instance 'bookmarks-update :rooms bookmarks)))))))
        
        ;; Catch-all
        (t nil))
    (error () nil)))

(defun engine-stop (engine)
  "Stop the XMPP engine."
  (setf (engine-running-p engine) nil)
  (when (engine-connection engine)
    (handler-case
        (xmpp:disconnect (engine-connection engine))
      (error () nil)))
  engine)

(defun engine-send-message (engine to body)
  "Send a message via the XMPP engine."
  (when (engine-connection engine)
    (handler-case
        (xmpp:message (engine-connection engine) to body)
      (error (e)
        (q-push (engine-queue engine)
                (make-instance 'error-event
                               :where :send
                               :condition (format nil "~a" e))))))
  (values to body))

(defun engine-send-groupchat (engine room-jid body)
  "Send a groupchat message to a MUC."
  (when (engine-connection engine)
    (handler-case
        (xmpp:message (engine-connection engine) room-jid body :type :groupchat)
      (error () nil))))

(defun engine-join-muc (engine room-jid &optional nickname)
  "Join a MUC room by sending presence to room-jid/nickname."
  (when (engine-connection engine)
    (let ((nick (or nickname (parse-jid (engine-jid engine)))))
      (handler-case
          ;; Send presence with MUC namespace to join
          (xmpp:presence (engine-connection engine)
                         :to (format nil "~a/~a" room-jid nick))
        (error () nil)))))

(defun engine-join-muc-raw (conn room-jid nickname)
  "Join a MUC room using raw connection (for use during startup).
   Sends presence with MUC namespace as required by XEP-0045."
  (handler-case
      ;; Use cl-xmpp's XML output directly
      (let ((stream (xmpp::server-stream conn)))
        (format stream "<presence to='~a/~a'><x xmlns='http://jabber.org/protocol/muc'/></presence>"
                room-jid nickname)
        (force-output stream))
    (error () nil)))

(defun engine-set-presence (engine show &optional status)
  "Set presence via the XMPP engine."
  (when (engine-connection engine)
    (handler-case
        (xmpp:presence (engine-connection engine) :show show :status status)
      (error () nil))))

(defun engine-request-bookmarks (conn)
  "Request bookmarks from the server for MUC rooms.
   Try XEP-0402 PEP Native Bookmarks first, then fall back to XEP-0049 Private Storage."
  (handler-case
      (progn
        ;; Try XEP-0402 PEP Native Bookmarks (modern Prosody uses this)
        ;; Format: <iq type='get'><pubsub xmlns='http://jabber.org/protocol/pubsub'>
        ;;           <items node='urn:xmpp:bookmarks:1'/></pubsub></iq>
        (xmpp:with-iq (conn :type "get" :id "bookmarks-pep")
          (cxml:with-element "pubsub"
            (cxml:attribute "xmlns" "http://jabber.org/protocol/pubsub")
            (cxml:with-element "items"
              (cxml:attribute "node" "urn:xmpp:bookmarks:1"))))
        ;; Also try XEP-0049 Private Storage as fallback
        (xmpp:with-iq (conn :type "get" :id "bookmarks-private")
          (cxml:with-element "query"
            (cxml:attribute "xmlns" "jabber:iq:private")
            (cxml:with-element "storage"
              (cxml:attribute "xmlns" "storage:bookmarks")))))
    (error () nil)))

(defun engine-extract-bookmarks (stanza)
  "Extract MUC conference bookmarks from an IQ result stanza (DOM version)."
  (handler-case
      (let ((xml-element (ignore-errors (slot-value stanza 'xmpp::xml-element))))
        (when xml-element
          (let ((rooms nil))
            ;; Walk the XML looking for conference elements
            (labels ((find-conferences (node)
                       (when (and node (dom:element-p node))
                         (let ((node-name (ignore-errors (dom:local-name node))))
                           (when (and node-name (string= node-name "conference"))
                             (let ((jid (dom:get-attribute node "jid"))
                                   (room-name (dom:get-attribute node "name"))
                                   (autojoin (dom:get-attribute node "autojoin")))
                               (when jid
                                 (push (list :jid jid
                                             :name room-name
                                             :autojoin (string= autojoin "true"))
                                       rooms)))))
                         (loop for child across (dom:child-nodes node)
                               do (find-conferences child)))))
              (find-conferences xml-element))
            rooms)))
    (error () nil)))

(defun engine-extract-bookmarks-from-xml (xml-element)
  "Extract MUC conference bookmarks from a cl-xmpp xml-element object."
  (handler-case
      (let ((rooms nil))
        ;; Walk the xml-element tree looking for conference elements
        (labels ((find-conferences (el)
                   (when el
                     (let ((name (ignore-errors (xmpp::name el))))
                       ;; Check if this is a conference element
                       (when (eq name :|conference|)
                         (let* ((attrs (ignore-errors (xmpp::attributes el)))
                                (jid (cdr (assoc :|jid| attrs)))
                                (room-name (cdr (assoc :|name| attrs)))
                                (autojoin (cdr (assoc :|autojoin| attrs))))
                           (when jid
                             (push (list :jid jid
                                         :name room-name
                                         :autojoin (string= autojoin "true"))
                                   rooms)))))
                     ;; Recurse into child elements
                     (dolist (child (ignore-errors (xmpp::elements el)))
                       (find-conferences child)))))
          (find-conferences xml-element))
        rooms)
    (error () nil)))
