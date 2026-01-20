;;;; connection.lisp - High-level XMPP connection management
;;;;
;;;; Provides a clean API for connecting, authenticating, and
;;;; sending/receiving stanzas.

(in-package #:clabber)

;;; ============================================================
;;; XMPP Connection Class
;;; ============================================================

(defclass xmpp-connection ()
  ((stream      :initform nil :accessor conn-stream)
   (jid         :initarg :jid :accessor conn-jid)
   (bound-jid   :initform nil :accessor conn-bound-jid)
   (resource    :initarg :resource :accessor conn-resource :initform "CLabber")
   (connected-p :initform nil :accessor conn-connected-p)
   (authenticated-p :initform nil :accessor conn-authenticated-p))
  (:documentation "High-level XMPP connection."))

(defmethod print-object ((c xmpp-connection) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~a ~a" 
            (or (conn-bound-jid c) (conn-jid c))
            (if (conn-connected-p c) "connected" "disconnected"))))

;;; ============================================================
;;; Connection Lifecycle
;;; ============================================================

(defun xmpp-connect (jid password &key host port (resource "CLabber") (mechanism :plain))
  "Connect to an XMPP server and authenticate.
   Returns an xmpp-connection object."
  (multiple-value-bind (user domain jid-resource) (parse-jid jid)
    (let* ((hostname (or host domain))
           (port-num (or port 5222))
           (res (or jid-resource resource))
           (conn (make-instance 'xmpp-connection 
                                :jid jid 
                                :resource res)))
      ;; Connect with plain TCP first (STARTTLS will upgrade)
      (setf (conn-stream conn) (xmpp-stream-connect hostname port-num :domain domain))
      (setf (conn-connected-p conn) t)
      
      ;; Open stream and get features
      (let ((features (xmpp-stream-open (conn-stream conn))))
        ;; Upgrade to TLS via STARTTLS
        (when (or (member :starttls features)
                  (member :starttls-required features))
          (xmpp-stream-starttls (conn-stream conn))
          ;; Restart stream after TLS upgrade
          (xmpp-stream-restart (conn-stream conn))))
      
      ;; Authenticate
      (xmpp-sasl-auth (conn-stream conn) user password :mechanism mechanism)
      (setf (conn-authenticated-p conn) t)
      
      ;; Restart stream after auth
      (xmpp-stream-restart (conn-stream conn))
      
      ;; Bind resource
      (debug-log "Binding resource: ~a" res)
      (let ((bound-jid (xmpp-bind-resource conn res)))
        (setf (conn-bound-jid conn) bound-jid)
        (debug-log "Bound JID: ~a" bound-jid))
      
      ;; Start session (if required)
      (debug-log "Starting session...")
      (xmpp-start-session conn)
      (debug-log "Session started")
      
      conn)))

(defun xmpp-disconnect (conn)
  "Disconnect from the XMPP server."
  (when (conn-stream conn)
    (xmpp-stream-disconnect (conn-stream conn)))
  (setf (conn-connected-p conn) nil
        (conn-authenticated-p conn) nil))

;;; ============================================================
;;; Resource Binding
;;; ============================================================

(defun xmpp-bind-resource (conn resource)
  "Bind a resource and return the full JID."
  (let* ((bind-query (make-xml-element "bind"
                                       :namespace +ns-bind+
                                       :children (list (make-xml-element "resource" 
                                                                         :text resource))))
         (iq (make-iq-stanza "set" :query bind-query)))
    (xmpp-stream-send (conn-stream conn) (stanza-xml-element iq))
    ;; Read response
    (let ((response (xmpp-stream-read-element (conn-stream conn))))
      (when response
        ;; Extract JID from bind result
        (let ((bind-el (xml-child response "bind")))
          (when bind-el
            (let ((jid-el (xml-child bind-el "jid")))
              (when jid-el
                (xml-text jid-el)))))))))

(defun stanza-xml-element (stanza)
  "Convert a stanza to an xml-element for sending."
  (typecase stanza
    (message-stanza
     (make-xml-element "message"
                       :attributes `(,@(when (stanza-to stanza) 
                                         `(("to" . ,(stanza-to stanza))))
                                     ,@(when (stanza-type stanza) 
                                         `(("type" . ,(stanza-type stanza))))
                                     ,@(when (stanza-id stanza) 
                                         `(("id" . ,(stanza-id stanza)))))
                       :children `(,@(when (message-body stanza)
                                       (list (make-xml-element "body" 
                                                               :text (message-body stanza)))))))
    (presence-stanza
     (make-xml-element "presence"
                       :attributes `(,@(when (stanza-to stanza) 
                                         `(("to" . ,(stanza-to stanza))))
                                     ,@(when (stanza-type stanza) 
                                         `(("type" . ,(stanza-type stanza)))))
                       :children `(,@(when (presence-show stanza)
                                       (list (make-xml-element "show" 
                                                               :text (presence-show stanza))))
                                   ,@(when (presence-status stanza)
                                       (list (make-xml-element "status" 
                                                               :text (presence-status stanza)))))))
    (iq-stanza
     (make-xml-element "iq"
                       :attributes `(("type" . ,(or (stanza-type stanza) "get"))
                                     ("id" . ,(or (stanza-id stanza) (generate-id)))
                                     ,@(when (stanza-to stanza) 
                                         `(("to" . ,(stanza-to stanza)))))
                       :children (when (iq-query stanza) 
                                   (list (iq-query stanza)))))
    (t stanza)))

(defun xmpp-start-session (conn)
  "Start an XMPP session (XEP-0078 compatibility)."
  (let* ((session-query (make-xml-element "session" :namespace +ns-session+))
         (iq (make-iq-stanza "set" :query session-query)))
    (xmpp-stream-send (conn-stream conn) (stanza-xml-element iq))
    ;; Read and discard response
    (xmpp-stream-read-element (conn-stream conn))))

;;; ============================================================
;;; Sending Stanzas
;;; ============================================================

(defun xmpp-send (conn stanza)
  "Send a stanza on the connection."
  (xmpp-stream-send (conn-stream conn) (stanza-xml-element stanza)))

(defun xmpp-send-message (conn to body &key (type "chat"))
  "Send a message."
  (xmpp-send conn (make-message-stanza to body :type type)))

(defun xmpp-send-groupchat (conn room-jid body)
  "Send a groupchat message to a MUC."
  (xmpp-send conn (make-message-stanza room-jid body :type "groupchat")))

(defun xmpp-send-presence (conn &key to type show status)
  "Send a presence stanza."
  (xmpp-send conn (make-presence-stanza :to to :type type :show show :status status)))

;;; ============================================================
;;; Receiving Stanzas
;;; ============================================================

(defun xmpp-receive (conn)
  "Receive and parse the next stanza. Returns a stanza object."
  (let ((el (xmpp-stream-read-element (conn-stream conn))))
    (when el
      (parse-stanza el))))

;;; ============================================================
;;; Roster
;;; ============================================================

(defun xmpp-get-roster (conn)
  "Request the roster from the server."
  (let* ((query (make-xml-element "query" :namespace +ns-roster+))
         (iq (make-iq-stanza "get" :query query)))
    (xmpp-send conn iq)))

;;; ============================================================
;;; MUC (XEP-0045)
;;; ============================================================

(defun xmpp-join-muc (conn room-jid &optional nickname)
  "Join a MUC room."
  (let* ((nick (or nickname (parse-jid (conn-jid conn))))
         (full-jid (format nil "~a/~a" room-jid nick))
         (x-el (make-xml-element "x" :namespace +ns-muc+))
         (presence (make-presence-stanza :to full-jid)))
    ;; Add MUC namespace element
    (setf (stanza-xml presence)
          (make-xml-element "presence"
                            :attributes `(("to" . ,full-jid))
                            :children (list x-el)))
    (xmpp-stream-send (conn-stream conn) (stanza-xml presence))))

(defun xmpp-leave-muc (conn room-jid &optional nickname)
  "Leave a MUC room."
  (let* ((nick (or nickname (parse-jid (conn-jid conn))))
         (full-jid (format nil "~a/~a" room-jid nick)))
    (xmpp-send-presence conn :to full-jid :type "unavailable")))

;;; ============================================================
;;; Bookmarks (XEP-0048 / XEP-0402)
;;; ============================================================

(defun xmpp-get-bookmarks (conn)
  "Request bookmarks from the server. Tries PEP first, then private storage."
  ;; Try XEP-0402 PEP Native Bookmarks
  (let* ((items (make-xml-element "items" 
                                  :attributes '(("node" . "urn:xmpp:bookmarks:1"))))
         (pubsub (make-xml-element "pubsub" 
                                   :namespace +ns-pubsub+
                                   :children (list items)))
         (iq (make-iq-stanza "get" :query pubsub :id "bookmarks-pep")))
    (xmpp-send conn iq))
  ;; Also try XEP-0049 Private Storage
  (let* ((storage (make-xml-element "storage" 
                                    :namespace +ns-bookmarks+))
         (query (make-xml-element "query" 
                                  :namespace +ns-private+
                                  :children (list storage)))
         (iq (make-iq-stanza "get" :query query :id "bookmarks-private")))
    (xmpp-send conn iq)))

(defun parse-bookmarks (iq-stanza)
  "Parse bookmarks from an IQ result stanza."
  (let ((rooms nil)
        (query (iq-query iq-stanza)))
    (when query
      (labels ((find-conferences (el)
                 (when (typep el 'xml-element)
                   (when (string= (xml-name el) "conference")
                     (let ((jid (xml-attr el "jid"))
                           (name (xml-attr el "name"))
                           (autojoin (xml-attr el "autojoin")))
                       (when jid
                         (push (list :jid jid
                                     :name name
                                     :autojoin (string= autojoin "true"))
                               rooms))))
                   (dolist (child (xml-children el))
                     (find-conferences child)))))
        (find-conferences query)))
    (nreverse rooms)))
