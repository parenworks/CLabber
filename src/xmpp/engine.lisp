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

(defun current-timestamp ()
  "Return current time as ISO 8601 string for message timestamps."
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

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
  ((queue       :initform (make-event-queue) :reader engine-queue)
   (thread      :initform nil :accessor engine-thread)
   (running     :initform nil :accessor engine-running-p)
   (connection  :initform nil :accessor engine-connection)
   (jid         :initform nil :accessor engine-jid)
   (resource    :initform "CLabber" :accessor engine-resource)
   (config-mucs :initform nil :accessor engine-config-mucs)
   (bookmarks-received :initform nil :accessor engine-bookmarks-received-p))
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
                       ;; Enable Message Carbons (XEP-0280)
                       (handler-case
                           (xmpp-enable-carbons conn)
                         (error () nil))
                       (debug-log "Enabled message carbons")
                       ;; Initialize OMEMO device (load from disk or generate new)
                       (unless *omemo-device*
                         (setf *omemo-device* (load-omemo-device))
                         (debug-log "OMEMO device initialized, ID: ~a" (device-id *omemo-device*)))
                       ;; Fetch our own device list first so we can merge when publishing
                       (when *omemo-device*
                         (debug-log "Fetching own device list before publishing...")
                         (fetch-omemo-device-list conn (bare-jid (conn-bound-jid conn)))
                         ;; Wait for the response to arrive and be processed
                         (sleep 2)
                         ;; Process any pending stanzas (device list responses)
                         (loop for stanza = (handler-case (xmpp-receive conn)
                                              (error () nil))
                               while stanza
                               do (engine-handle-stanza eng stanza))
                         (debug-log "Own device list cached: ~a" 
                                    (get-cached-device-list (bare-jid (conn-bound-jid conn))))
                         ;; Now publish with merged device list
                         (publish-omemo-keys conn *omemo-device*)
                         (debug-log "Published OMEMO keys"))
                       ;; Config MUCs are now a fallback - only used if no server bookmarks
                       ;; Store them for later use if bookmarks fail
                       (setf (engine-config-mucs eng) muc-rooms)
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
               (body (message-body stanza))
               (delay (message-delay stanza))
               (msg-id (stanza-id stanza))
               (msg-type (stanza-type stanza))
               (chat-state (message-chat-state stanza))
               (omemo-el (message-omemo-encrypted stanza)))
           ;; Handle chat state notifications (XEP-0085)
           (when (and from chat-state)
             (debug-log "Chat state from ~a: ~a" from chat-state)
             (q-push (engine-queue engine)
                     (make-instance 'chat-state-event
                                    :from from
                                    :state chat-state)))
           ;; Try OMEMO decryption if encrypted element present
           ;; Always prefer OMEMO over fallback body (many clients send a
           ;; fallback <body> like "I sent you an OMEMO encrypted message...")
           (when from
             (debug-log "Message processing: from=~a body=~a omemo=~a" 
                        from (not (null body)) (not (null omemo-el))))
           (when (and from omemo-el)
             (debug-log "OMEMO encrypted message from ~a (fallback body: ~a) ns=~a xml=~a" 
                        from (not (null body)) (xml-namespace omemo-el)
                        (subseq (serialize-xml omemo-el) 0 (min 500 (length (serialize-xml omemo-el)))))
             (let ((plaintext (omemo-decrypt-message omemo-el (bare-jid from))))
               (if plaintext
                   (progn
                     (debug-log "OMEMO decrypted: ~a" (subseq plaintext 0 (min 50 (length plaintext))))
                     (setf body plaintext))
                   (progn
                     ;; Decryption failed - re-publish our keys so sender discovers us
                     (debug-log "OMEMO decryption failed, re-publishing keys and fetching sender's keys")
                     (handler-case
                         (progn
                           (engine-publish-omemo-keys engine)
                           (engine-fetch-omemo-keys engine (bare-jid from)))
                       (error (e) (debug-log "OMEMO key refresh error: ~a" e)))
                     (unless body
                       (setf body "[OMEMO encrypted message - unable to decrypt. Keys re-published, ask sender to resend.]"))))))
           ;; Handle message body
           (when (and from body (> (length body) 0))
             (debug-log "Message from ~a type=~a: ~a" from msg-type (subseq body 0 (min 50 (length body))))
             (q-push (engine-queue engine)
                     (make-instance 'xmpp-message 
                                    :from from 
                                    :body body
                                    :timestamp (or delay (current-timestamp))
                                    :msg-id msg-id
                                    :msg-type msg-type)))))
        
        ;; Presence stanza
        (presence-stanza
         (let ((from (stanza-from stanza)))
           (when from
             (let* ((type- (stanza-type stanza))
                    (show (presence-show stanza))
                    (bare (bare-jid from))
                    (resource (jid-resource from))
                    (presence-show (or show
                                       (if (and type- (string= type- "unavailable"))
                                           "offline"
                                           "available"))))
               (debug-log "Presence from ~a: type=~a show=~a" from type- show)
               (cond
                 ;; Auto-accept subscription requests
                 ((and type- (string= type- "subscribe"))
                  (debug-log "Auto-accepting subscription from ~a" from)
                  (when (engine-connection engine)
                    (xmpp-accept-subscription (engine-connection engine) bare)
                    ;; Also subscribe back (mutual subscription)
                    (xmpp-send-presence (engine-connection engine) :to bare :type "subscribe")))
                 ;; MUC presence (has resource = nickname)
                 (resource
                  (q-push (engine-queue engine)
                          (make-instance 'muc-presence
                                         :room bare
                                         :nick resource
                                         :show presence-show)))
                 ;; Regular contact presence
                 (t
                  (q-push (engine-queue engine)
                          (make-instance 'xmpp-presence
                                         :jid from
                                         :show presence-show))))))))
        
        ;; IQ stanza
        (iq-stanza
         (let ((type- (stanza-type stanza))
               (query (iq-query stanza)))
           (debug-log "IQ type=~a id=~a" type- (stanza-id stanza))
           (when query
             (debug-log "  Query: name=~a ns=~a" 
                        (when (typep query 'xml-element) (xml-name query))
                        (when (typep query 'xml-element) (xml-namespace query))))
           ;; Handle roster pushes (type="set" from server)
           (when (and (string= type- "set") query
                      (typep query 'xml-element)
                      (string= (xml-name query) "query")
                      (or (string= (xml-namespace query) +ns-roster+)
                          (search "roster" (or (xml-namespace query) ""))))
             (engine-handle-roster-push engine query stanza))
           (when (and (string= type- "result") query)
             ;; Check for roster
             (when (and (typep query 'xml-element)
                        (string= (xml-name query) "query")
                        (or (string= (xml-namespace query) +ns-roster+)
                            (search "roster" (or (xml-namespace query) ""))))
               (engine-handle-roster engine query))
             ;; Check for bookmarks (only process first response to avoid duplicates)
             (unless (engine-bookmarks-received-p engine)
               (let ((bookmarks (parse-bookmarks stanza)))
                 (when bookmarks
                   (setf (engine-bookmarks-received-p engine) t)
                   (debug-log "Found ~d bookmarks (first response)" (length bookmarks))
                   ;; Log all bookmarks for debugging
                   (dolist (bookmark bookmarks)
                     (debug-log "  Bookmark: ~a autojoin=~a" 
                                (getf bookmark :jid) (getf bookmark :autojoin)))
                   ;; Auto-join rooms with autojoin=true
                   (dolist (bookmark bookmarks)
                     (let ((jid (getf bookmark :jid))
                           (autojoin (getf bookmark :autojoin)))
                       (when (and jid autojoin)
                         (debug-log "Auto-joining bookmark: ~a" jid)
                         (engine-join-muc engine jid))))
                   ;; Push event to update UI (include all bookmarks, not just autojoin)
                   (q-push (engine-queue engine)
                           (make-instance 'bookmarks-update :rooms bookmarks)))))
             ;; Check for OMEMO PubSub responses (device lists and bundles)
             (engine-handle-omemo-iq engine stanza))))
        
        ;; Raw XML element (stream errors, etc)
        (xml-element
         (debug-log "XML element: ~a" (xml-name stanza)))
        
        ;; Catch-all
        (t nil))
    (error (e)
      (debug-log "Error handling stanza: ~a" e))))

(defun engine-handle-roster-push (engine query stanza)
  "Handle a server-pushed roster update (IQ type=set).
   Acknowledges the push and updates the roster."
  ;; Acknowledge the roster push with an empty result IQ
  (when (engine-connection engine)
    (let ((iq-id (stanza-id stanza)))
      (when iq-id
        (handler-case
            (xmpp-stream-send (conn-stream (engine-connection engine))
                              (make-xml-element "iq"
                                                :attributes `(("type" . "result")
                                                              ("id" . ,iq-id))))
          (error () nil)))))
  ;; Process each item in the push
  (dolist (child (xml-children query))
    (when (and (typep child 'xml-element)
               (string= (xml-name child) "item"))
      (let ((jid (xml-attr child "jid"))
            (name (xml-attr child "name"))
            (subscription (xml-attr child "subscription")))
        (when jid
          (debug-log "Roster push: ~a name=~a sub=~a" jid name subscription)
          (if (and subscription (string= subscription "remove"))
              ;; Contact removed
              (q-push (engine-queue engine)
                      (make-instance 'roster-remove :jid jid))
              ;; Contact added or updated
              (q-push (engine-queue engine)
                      (make-instance 'roster-item-update
                                     :jid jid :name name
                                     :subscription subscription))))))))

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
;;; OMEMO IQ Handling
;;; ============================================================

(defun engine-handle-omemo-iq (engine stanza)
  "Handle IQ results that may contain OMEMO PubSub data (device lists or bundles)."
  (handler-case
      (let* ((from (stanza-from stanza))
             ;; For own PEP results, server omits 'from' - use our own JID
             (effective-from (or from
                                 (when (engine-connection engine)
                                   (conn-bound-jid (engine-connection engine)))))
             (query (iq-query stanza)))
        (debug-log "OMEMO IQ handler: from=~a effective-from=~a query=~a raw=~a" 
                    from effective-from (when query (xml-name query))
                    (when query (subseq (serialize-xml query) 0 (min 800 (length (serialize-xml query))))))
        (when (and effective-from (typep query 'xml-element)
                   (string= (xml-name query) "pubsub"))
          (let ((items-el (xml-child query "items")))
            (when items-el
              (let ((node (xml-attr items-el "node")))
                (debug-log "OMEMO IQ: node=~a" node)
                (when node
                  (cond
                    ;; Device list response (modern or legacy)
                    ((or (string= node +omemo-devices-node+)
                         (string= node +omemo-legacy-devices-node+))
                     (let ((item-el (xml-child items-el "item")))
                       (when item-el
                         (let ((device-ids (parse-device-list-xml item-el)))
                           (debug-log "OMEMO: Parsed device list: ~a" device-ids)
                           (when device-ids
                             (cache-device-list (bare-jid effective-from) device-ids)
                             (debug-log "OMEMO: Cached device list for ~a: ~a (node=~a)" 
                                        effective-from device-ids node))))))
                    ;; Bundle response (modern or legacy)
                    ((or (search +omemo-bundles-node+ node)
                         (search +omemo-legacy-bundles-node+ node))
                     (let* ((item-el (xml-child items-el "item"))
                            (device-id-str (xml-attr item-el "id"))
                            (device-id (when device-id-str 
                                         (parse-integer device-id-str :junk-allowed t))))
                       (when (and item-el device-id)
                         (let ((bundle (parse-bundle-xml item-el (bare-jid effective-from) device-id)))
                           (when bundle
                             (cache-bundle bundle)
                             (debug-log "OMEMO: Cached bundle for ~a device ~a (node=~a)" 
                                        effective-from device-id node)))))))))))))
    (error (e)
      (debug-log "Error handling OMEMO IQ: ~a" e))))

(defun engine-fetch-omemo-keys (engine jid)
  "Fetch OMEMO device list for JID, then fetch bundles for each device.
   This initiates the key discovery process needed before sending encrypted messages."
  (when (engine-connection engine)
    (handler-case
        (progn
          (fetch-omemo-device-list (engine-connection engine) jid)
          (debug-log "Requested OMEMO device list for ~a" jid))
      (error (e)
        (debug-log "Error fetching OMEMO keys for ~a: ~a" jid e)))))

(defun engine-fetch-omemo-bundles-for-jid (engine jid)
  "Fetch OMEMO bundles for all known devices of JID."
  (when (engine-connection engine)
    (let ((device-ids (get-cached-device-list jid)))
      (dolist (device-id device-ids)
        (unless (get-cached-bundle jid device-id)
          (handler-case
              (progn
                (fetch-omemo-bundle (engine-connection engine) jid device-id)
                (debug-log "Requested OMEMO bundle for ~a device ~a" jid device-id))
            (error (e)
              (debug-log "Error fetching bundle for ~a device ~a: ~a" jid device-id e))))))))

(defun engine-establish-omemo-session (engine jid)
  "Establish OMEMO sessions with all devices of JID.
   Requires device list and bundles to be cached first."
  (let ((device-ids (get-cached-device-list jid)))
    (dolist (device-id device-ids)
      (unless (get-session jid device-id)
        (let ((bundle (get-cached-bundle jid device-id)))
          (when bundle
            (handler-case
                (progn
                  (create-session-as-initiator jid device-id bundle)
                  (debug-log "Established OMEMO session with ~a device ~a" jid device-id))
              (error (e)
                (debug-log "Error establishing session with ~a device ~a: ~a" 
                           jid device-id e)))))))))

;;; ============================================================
;;; Sending
;;; ============================================================

(defun engine-send-message (engine to body)
  "Send a message via the XMPP engine.
   Automatically uses OMEMO encryption when a session exists for the recipient."
  (when (engine-connection engine)
    (handler-case
        (let ((bare (bare-jid to)))
          (if (omemo-enabled-for-jid-p bare)
              ;; Send OMEMO-encrypted
              (let ((encrypted-el (omemo-encrypt-message bare body)))
                (if encrypted-el
                    (progn
                      (debug-log "Sending OMEMO-encrypted message to ~a" to)
                      (xmpp-send-omemo-message (engine-connection engine) to encrypted-el))
                    ;; Fallback to plaintext if encryption failed
                    (progn
                      (debug-log "OMEMO encryption failed for ~a, sending plaintext" to)
                      (xmpp-send-message (engine-connection engine) to body))))
              ;; No OMEMO session, send plaintext
              (xmpp-send-message (engine-connection engine) to body)))
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
  "Join a MUC room and request message history."
  (when (engine-connection engine)
    (let ((nick (or nickname (parse-jid (engine-jid engine)))))
      (handler-case
          (progn
            (xmpp-join-muc (engine-connection engine) room-jid nick)
            ;; Request last 50 messages from archive
            (xmpp-query-mam (engine-connection engine) room-jid :max 50))
        (error () nil)))))

(defun engine-set-presence (engine show &optional status)
  "Set presence via the XMPP engine."
  (when (engine-connection engine)
    (handler-case
        (xmpp-send-presence (engine-connection engine) :show show :status status)
      (error () nil))))

(defun engine-send-chat-state (engine to state &key (type "chat"))
  "Send a chat state notification (XEP-0085).
   STATE is one of: active, composing, paused, inactive, gone."
  (when (engine-connection engine)
    (handler-case
        (xmpp-send-chat-state (engine-connection engine) to state :type type)
      (error () nil))))

(defun engine-add-contact (engine jid &optional name)
  "Add a contact to the roster and subscribe."
  (when (engine-connection engine)
    (handler-case
        (xmpp-add-contact (engine-connection engine) jid name)
      (error (e)
        (debug-log "Error adding contact ~a: ~a" jid e)))))

(defun engine-remove-contact (engine jid)
  "Remove a contact from the roster."
  (when (engine-connection engine)
    (handler-case
        (xmpp-remove-contact (engine-connection engine) jid)
      (error (e)
        (debug-log "Error removing contact ~a: ~a" jid e)))))

(defun engine-accept-subscription (engine jid)
  "Accept a subscription request from JID."
  (when (engine-connection engine)
    (handler-case
        (xmpp-accept-subscription (engine-connection engine) jid)
      (error (e)
        (debug-log "Error accepting subscription from ~a: ~a" jid e)))))

(defun engine-publish-omemo-keys (engine)
  "Publish OMEMO device list and key bundle to PEP."
  (when (and (engine-connection engine) *omemo-device*)
    (handler-case
        (publish-omemo-keys (engine-connection engine) *omemo-device*)
      (error (e)
        (debug-log "Error publishing OMEMO keys: ~a" e)))))
