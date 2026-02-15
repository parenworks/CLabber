(in-package #:clabber.xmpp)

;;; XMPP Connection Layer
;;; High-level API for connecting, authenticating, and sending/receiving stanzas

;;; ============================================================
;;; Debug Logging
;;; ============================================================

(defvar *debug-log-stream* nil)
(defvar *debug-log-path* "/tmp/clabber-debug.log")

(defun debug-log (format-string &rest args)
  (when *debug-log-stream*
    (apply #'format *debug-log-stream* format-string args)
    (terpri *debug-log-stream*)
    (force-output *debug-log-stream*)))

(defun open-debug-log ()
  (unless *debug-log-stream*
    (setf *debug-log-stream*
          (open *debug-log-path* :direction :output
                :if-exists :supersede :if-does-not-exist :create))))

(defun close-debug-log ()
  (when *debug-log-stream*
    (close *debug-log-stream*)
    (setf *debug-log-stream* nil)))

(defun current-timestamp ()
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

;;; ============================================================
;;; XMPP Connection Class
;;; ============================================================

(defclass xmpp-connection ()
  ((xmpp-stream :initform nil :accessor conn-stream)
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
  "Connect to an XMPP server and authenticate. Returns an xmpp-connection."
  (multiple-value-bind (user domain jid-resource) (parse-jid jid)
    (let* ((hostname (or host domain))
           (port-num (or port 5222))
           (res (or jid-resource resource))
           (conn (make-instance 'xmpp-connection :jid jid :resource res)))
      ;; Connect with plain TCP first (STARTTLS will upgrade)
      (setf (conn-stream conn) (xmpp-stream-connect hostname port-num :domain domain))
      (setf (conn-connected-p conn) t)
      ;; Open stream and get features
      (let ((features (xmpp-stream-open (conn-stream conn))))
        ;; Upgrade to TLS via STARTTLS
        (when (or (member :starttls features)
                  (member :starttls-required features))
          (xmpp-stream-starttls (conn-stream conn))
          (xmpp-stream-restart (conn-stream conn))))
      ;; Authenticate
      (xmpp-sasl-auth (conn-stream conn) user password :mechanism mechanism)
      (setf (conn-authenticated-p conn) t)
      ;; Restart stream after auth
      (xmpp-stream-restart (conn-stream conn))
      ;; Bind resource
      (let ((bound-jid (xmpp-bind-resource conn res)))
        (setf (conn-bound-jid conn) bound-jid))
      ;; Start session
      (xmpp-start-session conn)
      conn)))

(defun xmpp-disconnect (conn)
  "Disconnect from the XMPP server."
  (when (conn-stream conn)
    (xmpp-stream-disconnect (conn-stream conn)))
  (setf (conn-connected-p conn) nil
        (conn-authenticated-p conn) nil))

;;; ============================================================
;;; Resource Binding & Session
;;; ============================================================

(defun xmpp-bind-resource (conn resource)
  "Bind a resource and return the full JID."
  (let* ((bind-query (make-xml-element "bind"
                                       :namespace +ns-bind+
                                       :children (list (make-xml-element "resource" :text resource))))
         (iq (make-iq-stanza "set" :query bind-query)))
    (xmpp-stream-send (conn-stream conn) (stanza-to-xml iq))
    (let ((response (xmpp-stream-read-element (conn-stream conn))))
      (when response
        (let ((bind-el (xml-child response "bind")))
          (when bind-el
            (let ((jid-el (xml-child bind-el "jid")))
              (when jid-el (xml-text jid-el)))))))))

(defun xmpp-start-session (conn)
  "Start an XMPP session (XEP-0078 compatibility)."
  (let* ((session-query (make-xml-element "session" :namespace +ns-session+))
         (iq (make-iq-stanza "set" :query session-query)))
    (xmpp-stream-send (conn-stream conn) (stanza-to-xml iq))
    (xmpp-stream-read-element (conn-stream conn))))

;;; ============================================================
;;; Sending
;;; ============================================================

(defun xmpp-send (conn stanza)
  "Send a stanza on the connection."
  (xmpp-stream-send (conn-stream conn) (stanza-to-xml stanza)))

(defun xmpp-send-message (conn to body &key (type "chat"))
  "Send a message. Returns the stanza ID."
  (let ((stanza (make-message-stanza to body :type type)))
    (xmpp-send conn stanza)
    (stanza-id stanza)))

(defun xmpp-send-omemo-message (conn to omemo-encrypted-el &key (type "chat"))
  "Send an OMEMO-encrypted message."
  (let ((msg-el (make-xml-element "message"
                                  :attributes `(("to" . ,to) ("type" . ,type) ("id" . ,(generate-id)))
                                  :children (list
                                             omemo-encrypted-el
                                             (make-xml-element "encryption"
                                                               :namespace "urn:xmpp:eme:0"
                                                               :attributes '(("namespace" . "urn:xmpp:omemo:2")
                                                                             ("name" . "OMEMO")))
                                             (make-xml-element "store" :namespace "urn:xmpp:hints")))))
    (xmpp-stream-send (conn-stream conn) msg-el)))

(defun xmpp-send-groupchat (conn room-jid body)
  "Send a groupchat message. Returns the stanza ID."
  (let ((stanza (make-message-stanza room-jid body :type "groupchat")))
    (xmpp-send conn stanza)
    (stanza-id stanza)))

(defun xmpp-send-correction (conn to body replace-id &key (type "chat"))
  "Send a message correction (XEP-0308). Returns the new stanza ID."
  (let* ((id (generate-id))
         (msg-el (make-xml-element "message"
                   :attributes `(("to" . ,to) ("type" . ,type) ("id" . ,id))
                   :children (list
                     (make-xml-element "body" :text body)
                     (make-xml-element "replace"
                       :namespace "urn:xmpp:message-correct:0"
                       :attributes `(("id" . ,replace-id)))))))
    (xmpp-stream-send (conn-stream conn) msg-el)
    id))

(defun xmpp-send-presence (conn &key to type show status)
  (xmpp-send conn (make-presence-stanza :to to :type type :show show :status status)))

(defun xmpp-send-chat-state (conn to state &key (type "chat"))
  "Send a chat state notification (XEP-0085)."
  (let* ((state-el (make-xml-element state :namespace +ns-chatstates+))
         (msg (make-xml-element "message"
                                :attributes `(("to" . ,to) ("type" . ,type))
                                :children (list state-el))))
    (xmpp-stream-send (conn-stream conn) msg)))

;;; ============================================================
;;; Receiving
;;; ============================================================

(defun xmpp-receive (conn)
  "Receive and parse the next stanza."
  (let ((el (xmpp-stream-read-element (conn-stream conn))))
    (when el (parse-stanza el))))

;;; ============================================================
;;; Roster
;;; ============================================================

(defun xmpp-get-roster (conn)
  (let* ((query (make-xml-element "query" :namespace +ns-roster+))
         (iq (make-iq-stanza "get" :query query)))
    (xmpp-send conn iq)))

(defun xmpp-add-contact (conn jid &optional name)
  (let* ((item-attrs (if name `(("jid" . ,jid) ("name" . ,name)) `(("jid" . ,jid))))
         (item (make-xml-element "item" :attributes item-attrs))
         (query (make-xml-element "query" :namespace +ns-roster+ :children (list item)))
         (iq (make-iq-stanza "set" :query query)))
    (xmpp-send conn iq))
  (xmpp-send-presence conn :to jid :type "subscribe"))

(defun xmpp-accept-subscription (conn jid)
  (xmpp-send-presence conn :to jid :type "subscribed"))

;;; ============================================================
;;; XMPP Ping / Keepalive (XEP-0199)
;;; ============================================================

(defun xmpp-send-ping (conn)
  "Send an XMPP ping to the server to keep the connection alive."
  (let* ((jid (bare-jid (or (conn-bound-jid conn) (conn-jid conn))))
         (server (when jid
                   (let ((at (position #\@ jid)))
                     (when at (subseq jid (1+ at))))))
         (ping-el (make-xml-element "ping" :namespace "urn:xmpp:ping"))
         (iq (make-iq-stanza "get" :to server :query ping-el
                             :id (format nil "ping-~a" (random 100000)))))
    (xmpp-send conn iq)))

;;; ============================================================
;;; Message Carbons (XEP-0280)
;;; ============================================================

(defun xmpp-enable-carbons (conn)
  (let* ((enable-el (make-xml-element "enable" :namespace +ns-carbons+))
         (iq (make-iq-stanza "set" :query enable-el :id "carbons-1")))
    (xmpp-send conn iq)))

;;; ============================================================
;;; Service Discovery (XEP-0030)
;;; ============================================================

(defun xmpp-disco-info (conn jid)
  "Send a disco#info query to JID."
  (let* ((query (make-xml-element "query" :namespace +ns-disco-info+))
         (iq (make-iq-stanza "get" :to jid :query query
                             :id (format nil "disco-~a" (random 100000)))))
    (xmpp-send conn iq)))

;;; ============================================================
;;; MUC (XEP-0045)
;;; ============================================================

(defun xmpp-join-muc (conn room-jid &optional nickname)
  (let* ((nick (or nickname (parse-jid (conn-jid conn))))
         (full-jid (format nil "~a/~a" room-jid nick))
         (x-el (make-xml-element "x" :namespace +ns-muc+)))
    (xmpp-stream-send (conn-stream conn)
                       (make-xml-element "presence"
                                         :attributes `(("to" . ,full-jid))
                                         :children (list x-el)))))

(defun xmpp-leave-muc (conn room-jid &optional nickname)
  (let* ((nick (or nickname (parse-jid (conn-jid conn))))
         (full-jid (format nil "~a/~a" room-jid nick)))
    (xmpp-send-presence conn :to full-jid :type "unavailable")))

;;; ============================================================
;;; Bookmarks (XEP-0048 / XEP-0402)
;;; ============================================================

(defun xmpp-get-bookmarks (conn)
  ;; XEP-0402 PEP Native Bookmarks
  (let* ((items (make-xml-element "items" :attributes '(("node" . "urn:xmpp:bookmarks:1"))))
         (pubsub (make-xml-element "pubsub" :namespace +ns-pubsub+ :children (list items)))
         (iq (make-iq-stanza "get" :query pubsub :id "bookmarks-pep")))
    (xmpp-send conn iq))
  ;; XEP-0049 Private Storage fallback
  (let* ((storage (make-xml-element "storage" :namespace +ns-bookmarks+))
         (query (make-xml-element "query" :namespace +ns-private+ :children (list storage)))
         (iq (make-iq-stanza "get" :query query :id "bookmarks-private")))
    (xmpp-send conn iq)))

(defun parse-bookmarks (iq-stanza)
  "Parse bookmarks from an IQ result stanza."
  (let ((rooms nil)
        (query (iq-query iq-stanza)))
    (when query
      (labels ((find-conferences (el &optional parent-item-id)
                 (when (typep el 'xml-element)
                   (let ((el-name (xml-name el)))
                     (when (string= el-name "item")
                       (let ((item-id (xml-attr el "id")))
                         (dolist (child (xml-children el))
                           (find-conferences child item-id))
                         (return-from find-conferences)))
                     (when (string= el-name "conference")
                       (let ((jid (or (xml-attr el "jid") parent-item-id))
                             (name (xml-attr el "name"))
                             (autojoin (xml-attr el "autojoin")))
                         (when jid
                           (push (list :jid jid :name (or name jid)
                                       :autojoin (or (string= autojoin "true")
                                                     (string= autojoin "1")))
                                 rooms))))
                     (dolist (child (xml-children el))
                       (find-conferences child parent-item-id))))))
        (find-conferences query)))
    (nreverse rooms)))

;;; ============================================================
;;; PEP / PubSub (XEP-0163)
;;; ============================================================

(defun xmpp-pep-publish (conn node item-id item-children)
  "Publish an item to a PEP node."
  (let* ((item (make-xml-element "item"
                 :attributes (when item-id `(("id" . ,item-id)))
                 :children item-children))
         (publish (make-xml-element "publish"
                    :attributes `(("node" . ,node))
                    :children (list item)))
         (pubsub (make-xml-element "pubsub"
                   :namespace +ns-pubsub+
                   :children (list publish)))
         (iq (make-iq-stanza "set" :query pubsub :id (format nil "pep-~a" (random 100000)))))
    (xmpp-send conn iq)))

(defun xmpp-pep-fetch (conn jid node)
  "Fetch items from a PEP node for JID."
  (let* ((items (make-xml-element "items"
                  :attributes `(("node" . ,node))))
         (pubsub (make-xml-element "pubsub"
                   :namespace +ns-pubsub+
                   :children (list items)))
         (iq (make-iq-stanza "get" :to jid :query pubsub
                             :id (format nil "pep-fetch-~a" (random 100000)))))
    (xmpp-send conn iq)))

(defun xmpp-publish-omemo-devicelist (conn device-id &optional existing-device-ids)
  "Publish our OMEMO device list via PEP, merging with EXISTING-DEVICE-IDS."
  (let* ((all-ids (remove-duplicates (cons device-id (or existing-device-ids nil))))
         (device-elements (mapcar (lambda (id)
                                    (make-xml-element "device"
                                      :attributes `(("id" . ,(princ-to-string id)))))
                                  all-ids))
         (list-el (make-xml-element "list"
                    :namespace "eu.siacs.conversations.axolotl"
                    :children device-elements)))
    (xmpp-pep-publish conn "eu.siacs.conversations.axolotl.devicelist"
                      "current" (list list-el))))

(defun xmpp-publish-omemo-bundle (conn device-id bundle-xml-element)
  "Publish our OMEMO bundle via PEP."
  (let ((node (format nil "eu.siacs.conversations.axolotl.bundles:~a" device-id)))
    (xmpp-pep-publish conn node "current" (list bundle-xml-element))))

(defun xmpp-fetch-omemo-devicelist (conn jid)
  "Fetch OMEMO device list for JID."
  (xmpp-pep-fetch conn jid "eu.siacs.conversations.axolotl.devicelist"))

(defun xmpp-fetch-omemo-bundle (conn jid device-id)
  "Fetch OMEMO bundle for JID/device-id."
  (xmpp-pep-fetch conn jid
                  (format nil "eu.siacs.conversations.axolotl.bundles:~a" device-id)))

;;; ============================================================
;;; MAM (XEP-0313)
;;; ============================================================

(defun xmpp-query-mam (conn jid &key max with)
  "Query MAM archive. For MUCs, JID is the room. For DMs, JID is nil and WITH is the contact JID."
  (let* ((query-id (format nil "mam-~a" (random 100000)))
         (rsm-children (list (make-xml-element "before")))
         (rsm-children (if max
                           (cons (make-xml-element "max" :text (princ-to-string max))
                                 rsm-children)
                           rsm-children))
         (rsm (make-xml-element "set"
                                :namespace "http://jabber.org/protocol/rsm"
                                :children rsm-children))
         ;; Build data form with 'with' filter for DM queries
         (form-children
           (when with
             (list (make-xml-element "x"
                     :namespace "jabber:x:data"
                     :attributes '(("type" . "submit"))
                     :children (list
                       (make-xml-element "field"
                         :attributes '(("var" . "FORM_TYPE") ("type" . "hidden"))
                         :children (list (make-xml-element "value" :text +ns-mam+)))
                       (make-xml-element "field"
                         :attributes '(("var" . "with"))
                         :children (list (make-xml-element "value" :text with))))))))
         (query (make-xml-element "query"
                                  :namespace +ns-mam+
                                  :attributes `(("queryid" . ,query-id))
                                  :children (append form-children (list rsm))))
         (iq (make-iq-stanza "set" :to jid :query query :id query-id)))
    (xmpp-send conn iq)
    query-id))
