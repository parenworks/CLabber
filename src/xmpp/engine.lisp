;;;; engine-new.lisp - XMPP engine using native implementation
;;;;
;;;; This replaces the cl-xmpp based engine with our own XMPP protocol code.

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

;;; ============================================================
;;; JID Parsing
;;; ============================================================

(defun parse-jid (jid)
  "Parse JID into (values user domain resource)."
  (let* ((at-pos (position #\@ jid))
         (slash-pos (position #\/ jid)))
    (values (if at-pos (subseq jid 0 at-pos) jid)
            (if at-pos
                (subseq jid (1+ at-pos) (or slash-pos (length jid)))
                nil)
            (if slash-pos (subseq jid (1+ slash-pos)) nil))))

(defun bare-jid (full-jid)
  "Extract bare JID (user@domain) from full JID."
  (let ((slash-pos (position #\/ full-jid)))
    (if slash-pos
        (subseq full-jid 0 slash-pos)
        full-jid)))

(defun jid-resource (full-jid)
  "Extract resource from full JID."
  (let ((slash-pos (position #\/ full-jid)))
    (when slash-pos
      (subseq full-jid (1+ slash-pos)))))

;;; ============================================================
;;; XMPP Engine Class
;;; ============================================================

(defclass xmpp-engine ()
  ((queue      :initform (make-event-queue) :reader engine-queue)
   (thread     :initform nil :accessor engine-thread)
   (running    :initform nil :accessor engine-running-p)
   (connection :initform nil :accessor engine-connection)
   (jid        :initform nil :accessor engine-jid)
   (resource   :initform "CLabber" :accessor engine-resource))
  (:documentation "XMPP engine using native protocol implementation."))

;;; ============================================================
;;; Engine Lifecycle
;;; ============================================================

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
               (open-debug-log)
               (debug-log "=== CLabber XMPP Engine Starting (Native) ===")
               (debug-log "JID: ~a, Host: ~a, Port: ~d" jid (or hst dom) prt)
               (handler-case
                   (progn
                     ;; Notify connecting
                     (q-push (engine-queue eng)
                             (make-instance 'xmpp-connecting :jid jid))
                     ;; Connect and authenticate
                     (let ((conn (xmpp-connect jid pwd
                                               :host hst
                                               :port prt
                                               :resource res
                                               :mechanism :plain)))
                       (setf (engine-connection eng) conn)
                       (debug-log "Connected and authenticated as ~a" (conn-bound-jid conn))
                       ;; Notify connected
                       (q-push (engine-queue eng)
                               (make-instance 'xmpp-connected 
                                              :jid (or (conn-bound-jid conn) jid)))
                       ;; Send initial presence
                       (xmpp-send-presence conn)
                       (debug-log "Sent initial presence")
                       ;; Request roster
                       (xmpp-get-roster conn)
                       (debug-log "Requested roster")
                       ;; Request bookmarks
                       (xmpp-get-bookmarks conn)
                       (debug-log "Requested bookmarks")
                       ;; Auto-join MUC rooms from config
                       (when muc-rooms
                         (debug-log "Auto-joining ~d MUC rooms from config" (length muc-rooms))
                         (dolist (room muc-rooms)
                           (debug-log "Joining MUC: ~a" room)
                           (xmpp-join-muc conn room usr))
                         ;; Add all MUC rooms to roster
                         (q-push (engine-queue eng)
                                 (make-instance 'bookmarks-update
                                                :rooms (mapcar (lambda (room)
                                                                 (list :jid room :name room :autojoin t))
                                                               muc-rooms))))
                       ;; Enter receive loop
                       (engine-receive-loop eng)))
                 (error (e)
                   (debug-log "Connection error: ~a" e)
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
                     (conn-connected-p conn))
          do (handler-case
                 (let ((stanza (xmpp-receive conn)))
                   (when stanza
                     (incf stanza-count)
                     (debug-log "Stanza #~d: ~a" stanza-count (type-of stanza))
                     (engine-handle-stanza engine stanza)))
               (error (e)
                 (debug-log "Receive error: ~a" e)
                 ;; Don't break loop on transient errors
                 (sleep 0.1)))))
  ;; Connection ended
  (debug-log "Receive loop ended")
  (q-push (engine-queue engine)
          (make-instance 'xmpp-disconnected)))

(defun engine-stop (engine)
  "Stop the XMPP engine."
  (setf (engine-running-p engine) nil)
  (when (engine-connection engine)
    (handler-case
        (xmpp-disconnect (engine-connection engine))
      (error () nil)))
  (close-debug-log)
  engine)

;;; ============================================================
;;; Stanza Handling
;;; ============================================================

(defun engine-handle-stanza (engine stanza)
  "Handle a received XMPP stanza and convert to events."
  (handler-case
      (typecase stanza
        ;; Message stanza
        (message-stanza
         (let ((from (stanza-from stanza))
               (body (message-body stanza)))
           (when (and from body (> (length body) 0))
             (debug-log "Message from ~a: ~a" from (subseq body 0 (min 50 (length body))))
             (q-push (engine-queue engine)
                     (make-instance 'xmpp-message :from from :body body)))))
        
        ;; Presence stanza
        (presence-stanza
         (let ((from (stanza-from stanza)))
           (when from
             (let ((type- (stanza-type stanza))
                   (show (presence-show stanza)))
               (debug-log "Presence from ~a: type=~a show=~a" from type- show)
               (q-push (engine-queue engine)
                       (make-instance 'xmpp-presence
                                      :jid from
                                      :show (or show
                                                (if (and type- (string= type- "unavailable"))
                                                    "offline"
                                                    "available"))))))))
        
        ;; IQ stanza
        (iq-stanza
         (let ((type- (stanza-type stanza))
               (query (iq-query stanza)))
           (debug-log "IQ type=~a id=~a" type- (stanza-id stanza))
           (when (and (string= type- "result") query)
             ;; Check for roster
             (when (and (typep query 'xml-element)
                        (string= (xml-name query) "query")
                        (string= (xml-namespace query) +ns-roster+))
               (engine-handle-roster engine query))
             ;; Check for bookmarks
             (let ((bookmarks (parse-bookmarks stanza)))
               (when bookmarks
                 (debug-log "Found ~d bookmarks" (length bookmarks))
                 (q-push (engine-queue engine)
                         (make-instance 'bookmarks-update :rooms bookmarks)))))))
        
        ;; Raw XML element (stream errors, etc)
        (xml-element
         (debug-log "XML element: ~a" (xml-name stanza)))
        
        ;; Catch-all
        (t nil))
    (error (e)
      (debug-log "Error handling stanza: ~a" e))))

(defun engine-handle-roster (engine query)
  "Handle a roster query result."
  (let ((items nil))
    (dolist (child (xml-children query))
      (when (and (typep child 'xml-element)
                 (string= (xml-name child) "item"))
        (let ((jid (xml-attr child "jid"))
              (name (xml-attr child "name")))
          (when jid
            (push (make-instance 'roster-item :jid jid :name name) items)))))
    (when items
      (debug-log "Roster: ~d items" (length items))
      (q-push (engine-queue engine)
              (make-instance 'roster-update :items (nreverse items))))))

;;; ============================================================
;;; Sending
;;; ============================================================

(defun engine-send-message (engine to body)
  "Send a message via the XMPP engine."
  (when (engine-connection engine)
    (handler-case
        (xmpp-send-message (engine-connection engine) to body)
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
        (xmpp-send-groupchat (engine-connection engine) room-jid body)
      (error () nil))))

(defun engine-join-muc (engine room-jid &optional nickname)
  "Join a MUC room."
  (when (engine-connection engine)
    (let ((nick (or nickname (parse-jid (engine-jid engine)))))
      (handler-case
          (xmpp-join-muc (engine-connection engine) room-jid nick)
        (error () nil)))))

(defun engine-set-presence (engine show &optional status)
  "Set presence via the XMPP engine."
  (when (engine-connection engine)
    (handler-case
        (xmpp-send-presence (engine-connection engine) :show show :status status)
      (error () nil))))
