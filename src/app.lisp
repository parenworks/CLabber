(in-package #:clabber)

;;; Application Class and Event Loop
;;; Manages all application state, rendering, and input handling

(defclass application ()
  ((config :initarg :config :accessor app-config :initform nil)
   (layout :initarg :layout :accessor app-layout :initform nil)
   (buffers :initform (make-array 16 :adjustable t :fill-pointer 0)
            :accessor app-buffers)
   (active-buffer :initform 0 :accessor app-active-buffer)
   (connections :initform (make-hash-table :test 'equal)
                :accessor app-connections
                :documentation "JID -> xmpp-connection")
   (contacts :initform (make-hash-table :test 'equal)
             :accessor app-contacts
             :documentation "JID -> contact")
   (running-p :initform nil :accessor app-running-p)
   (width :initform 80 :accessor app-width)
   (height :initform 24 :accessor app-height)
   (splash :initform nil :accessor app-splash)
   (receive-thread :initform nil :accessor app-receive-thread)
   (event-queue :initform (bt:make-lock "event-queue-lock") :accessor app-event-lock)
   (events :initform nil :accessor app-events
           :documentation "List of pending events from receive thread")
   (bookmarks-received-p :initform nil :accessor app-bookmarks-received-p)
   ;; TAB completion state
   (tab-prefix :initform nil :accessor app-tab-prefix
               :documentation "The partial nick being completed")
   (tab-matches :initform nil :accessor app-tab-matches
                :documentation "List of matching nicks")
   (tab-index :initform 0 :accessor app-tab-index
              :documentation "Current index into tab-matches")
   (tab-word-start :initform 0 :accessor app-tab-word-start
                   :documentation "Start position of the word being completed")
   (tab-after-pos :initform 0 :accessor app-tab-after-pos
                  :documentation "Position after the last inserted completion"))
  (:documentation "Main CLabber application state"))

(defun make-app ()
  (make-instance 'application))

(defun app-first-connection (app)
  "Get the first (primary) XMPP connection."
  (let ((conn nil))
    (maphash (lambda (k v) (declare (ignore k)) (unless conn (setf conn v)))
             (app-connections app))
    conn))

(defun app-push-event (app event)
  "Thread-safe push of an event from the receive thread."
  (bt:with-lock-held ((app-event-lock app))
    (push event (app-events app))))

(defun app-drain-events (app)
  "Drain all pending events (called from main thread)."
  (bt:with-lock-held ((app-event-lock app))
    (let ((evts (nreverse (app-events app))))
      (setf (app-events app) nil)
      evts)))

(defun app-current-buffer (app)
  "Get the currently active buffer."
  (let ((idx (app-active-buffer app))
        (bufs (app-buffers app)))
    (when (< idx (fill-pointer bufs))
      (aref bufs idx))))

(defun app-add-buffer (app buffer)
  "Add a buffer to the application."
  (vector-push-extend buffer (app-buffers app))
  buffer)

(defun app-switch-buffer (app direction)
  "Switch to next/previous buffer. DIRECTION is :next or :prev."
  (let ((count (fill-pointer (app-buffers app)))
        (idx (app-active-buffer app)))
    (when (> count 0)
      (setf (app-active-buffer app)
            (mod (+ idx (if (eq direction :next) 1 -1)) count)))))

(defun app-dm-buffers (app)
  "Get list of DM buffers."
  (loop for i from 0 below (fill-pointer (app-buffers app))
        for buf = (aref (app-buffers app) i)
        when (eq (buffer-type buf) :dm)
        collect buf))

(defun app-muc-buffers (app)
  "Get list of MUC buffers."
  (loop for i from 0 below (fill-pointer (app-buffers app))
        for buf = (aref (app-buffers app) i)
        when (eq (buffer-type buf) :muc)
        collect buf))

;;; ============================================================
;;; Rendering
;;; ============================================================

(defun app-update-layout (app)
  "Recompute layout based on current terminal size."
  (let ((size (terminal-size)))
    (setf (app-width app) (first size)
          (app-height app) (second size))
    (unless (app-layout app)
      (setf (app-layout app) (make-layout)))
    (layout-compute (app-layout app) (app-width app) (app-height app))))

(defun app-render (app)
  "Render the full UI."
  (let ((ly (app-layout app))
        (buf (app-current-buffer app)))
    ;; Update roster panel
    (when (layout-roster ly)
      (let ((dms (app-dm-buffers app))
            (mucs (app-muc-buffers app)))
        (setf (roster-dm-buffers (layout-roster ly)) dms)
        (setf (roster-muc-buffers (layout-roster ly)) mucs)
        ;; Compute roster-relative selected index
        ;; DMs are 0..n-1, MUCs are n..n+m-1
        (let ((roster-idx nil))
          (loop for i from 0 for b in dms
                when (eq b buf) do (setf roster-idx i))
          (unless roster-idx
            (loop for i from 0 for b in mucs
                  when (eq b buf) do (setf roster-idx (+ (length dms) i))))
          (setf (roster-selected-index (layout-roster ly)) (or roster-idx -1)))))
    ;; Update chat panel
    (when (and buf (layout-chat-a ly))
      (setf (chat-panel-buffer (layout-chat-a ly)) buf)
      (setf (panel-active-p (layout-chat-a ly)) t))
    ;; Update participants panel
    (when (and buf (layout-participants ly))
      (setf (participants-list (layout-participants ly))
            (when (eq (buffer-type buf) :muc)
              (buffer-participants buf))))
    ;; Update status bar
    (when (layout-status ly)
      (setf (status-bar-left (layout-status ly))
            (if buf
                (format nil " ~A" (or (buffer-display-name buf) (buffer-name buf)))
                " CLabber"))
      (setf (status-bar-right (layout-status ly))
            (format nil "~A " (if (and buf (buffer-omemo-p buf)) "OMEMO" ""))))
    ;; Update buffer bar
    (when (layout-buffer-bar ly)
      (setf (buffer-bar-buffers (layout-buffer-bar ly))
            (coerce (app-buffers app) 'list))
      (setf (buffer-bar-active-index (layout-buffer-bar ly))
            (app-active-buffer app)))
    ;; Render all
    (layout-render-all ly)))

;;; ============================================================
;;; Input handling
;;; ============================================================

(defun app-handle-input (app key)
  "Handle a key event. Returns NIL to quit, T to continue."
  ;; Reset TAB completion state on any non-TAB key
  (unless (eql (key-event-code key) +key-tab+)
    (setf (app-tab-prefix app) nil
          (app-tab-matches app) nil
          (app-tab-index app) 0))
  (let ((ly (app-layout app)))
    (cond
      ;; Ctrl-Q or F10: quit
      ((and (key-event-ctrl-p key) (eql (key-event-char key) #\q))
       nil)
      ;; Ctrl-N: next buffer
      ((and (key-event-ctrl-p key) (eql (key-event-char key) #\n))
       (app-switch-buffer app :next) t)
      ;; Ctrl-P: previous buffer
      ((and (key-event-ctrl-p key) (eql (key-event-char key) #\p))
       (app-switch-buffer app :prev) t)
      ;; Ctrl-W: cycle split mode
      ((and (key-event-ctrl-p key) (eql (key-event-char key) #\w))
       (let ((current (layout-split-mode (app-layout app))))
         (setf (layout-split-mode (app-layout app))
               (case current
                 ((nil) :horizontal)
                 (:horizontal :vertical)
                 (:vertical nil))))
       (app-update-layout app) t)
      ;; Up arrow: previous buffer
      ((eql (key-event-code key) +key-up+)
       (app-switch-buffer app :prev) t)
      ;; Down arrow: next buffer
      ((eql (key-event-code key) +key-down+)
       (app-switch-buffer app :next) t)
      ;; Scroll up
      ((eql (key-event-code key) +key-page-up+)
       (let ((buf (app-current-buffer app)))
         (when buf (incf (buffer-scroll-offset buf) 5)))
       t)
      ;; Scroll down
      ((eql (key-event-code key) +key-page-down+)
       (let ((buf (app-current-buffer app)))
         (when buf (setf (buffer-scroll-offset buf)
                         (max 0 (- (buffer-scroll-offset buf) 5)))))
       t)
      ;; TAB: nick completion
      ((eql (key-event-code key) +key-tab+)
       (let ((input (layout-input ly))
             (buf (app-current-buffer app)))
         (when (and input buf (eq (buffer-type buf) :muc)
                    (buffer-participants buf))
           (if (app-tab-matches app)
               ;; Cycling through existing matches
               (let* ((text (input-bar-text input))
                      (word-start (app-tab-word-start app))
                      (match (nth (app-tab-index app) (app-tab-matches app)))
                      (suffix (if (zerop word-start) ": " " "))
                      (after-pos (app-tab-after-pos app))
                      (new-text (concatenate 'string
                                             (subseq text 0 word-start)
                                             match suffix
                                             (if (< after-pos (length text))
                                                 (subseq text after-pos)
                                                 "")))
                      (new-pos (+ word-start (length match) (length suffix))))
                 (setf (input-bar-text input) new-text
                       (input-bar-cursor-pos input) new-pos
                       (app-tab-after-pos app) new-pos)
                 (setf (app-tab-index app)
                       (mod (1+ (app-tab-index app))
                            (length (app-tab-matches app)))))
               ;; First TAB: find matches
               (let* ((text (input-bar-text input))
                      (pos (input-bar-cursor-pos input))
                      (space-pos (position #\Space text :end pos :from-end t))
                      (word-start (if space-pos (1+ space-pos) 0))
                      (partial (subseq text word-start pos)))
                 (when (> (length partial) 0)
                   (let ((matches (sort (remove-if-not
                                         (lambda (nick)
                                           (and (>= (length nick) (length partial))
                                                (string-equal (subseq nick 0 (length partial)) partial)))
                                         (copy-list (buffer-participants buf)))
                                        #'string<)))
                     (when matches
                       (setf (app-tab-prefix app) partial
                             (app-tab-matches app) matches
                             (app-tab-index app) 0
                             (app-tab-word-start app) word-start
                             (app-tab-after-pos app) pos)
                       ;; Insert first match
                       (let* ((match (first matches))
                              (suffix (if (zerop word-start) ": " " "))
                              (new-text (concatenate 'string
                                                     (subseq text 0 word-start)
                                                     match suffix
                                                     (subseq text pos)))
                              (new-pos (+ word-start (length match) (length suffix))))
                         (setf (input-bar-text input) new-text
                               (input-bar-cursor-pos input) new-pos
                               (app-tab-after-pos app) new-pos)
                         (setf (app-tab-index app)
                               (mod 1 (length matches)))))))))))
       t)
      ;; Enter: send message
      ((eql (key-event-code key) +key-enter+)
       (let ((input (layout-input ly)))
         (when (and input (> (length (input-bar-text input)) 0))
           (let ((text (input-bar-text input)))
             ;; Handle commands
             (if (and (> (length text) 0) (char= (char text 0) #\/))
                 (app-handle-command app text)
                 ;; Send message
                 (app-send-message app text))
             (setf (input-bar-text input) ""
                   (input-bar-cursor-pos input) 0))))
       t)
      ;; Backspace
      ((eql (key-event-code key) +key-backspace+)
       (let ((input (layout-input ly)))
         (when (and input (> (input-bar-cursor-pos input) 0))
           (let ((pos (input-bar-cursor-pos input))
                 (text (input-bar-text input)))
             (setf (input-bar-text input)
                   (concatenate 'string (subseq text 0 (1- pos)) (subseq text pos)))
             (decf (input-bar-cursor-pos input)))))
       t)
      ;; Left arrow
      ((eql (key-event-code key) +key-left+)
       (let ((input (layout-input ly)))
         (when (and input (> (input-bar-cursor-pos input) 0))
           (decf (input-bar-cursor-pos input))))
       t)
      ;; Right arrow
      ((eql (key-event-code key) +key-right+)
       (let ((input (layout-input ly)))
         (when (and input (< (input-bar-cursor-pos input) (length (input-bar-text input))))
           (incf (input-bar-cursor-pos input))))
       t)
      ;; Regular character
      ((key-event-char key)
       (let ((input (layout-input ly))
             (ch (key-event-char key)))
         (when (and input (graphic-char-p ch))
           (let ((pos (input-bar-cursor-pos input))
                 (text (input-bar-text input)))
             (setf (input-bar-text input)
                   (concatenate 'string (subseq text 0 pos) (string ch) (subseq text pos)))
             (incf (input-bar-cursor-pos input)))))
       t)
      ;; Unknown key - ignore
      (t t))))

(defun app-handle-command (app text)
  "Handle a /command."
  (let* ((parts (cl-ppcre:split "\\s+" text :limit 2))
         (cmd (string-downcase (first parts)))
         (args (second parts)))
    (cond
      ((string= cmd "/quit")
       (setf (app-running-p app) nil))
      ((string= cmd "/join")
       (when args
         ;; TODO: XMPP MUC join
         (let ((buf (make-buffer :name args
                                 :display-name (strip-muc-name args)
                                 :type :muc)))
           (app-add-buffer app buf))))
      ((string= cmd "/msg")
       (when args
         (let* ((space (position #\Space args))
                (jid (if space (subseq args 0 space) args)))
           (unless (find jid (coerce (app-buffers app) 'list)
                         :key #'buffer-name :test #'string=)
             (app-add-buffer app (make-buffer :name jid :type :dm))))))
      ((string= cmd "/omemo")
       (let ((buf (app-current-buffer app)))
         (when (and buf (not (eq (buffer-type buf) :system)))
           (let ((new-state (not (buffer-omemo-p buf))))
             (setf (buffer-omemo-p buf) new-state)
             (buffer-add-message buf
               (make-message :text (if new-state
                                       "🔒 OMEMO encryption enabled"
                                       "🔓 OMEMO encryption disabled")
                             :level :system))
             ;; If enabling, fetch device list for the contact
             (when (and new-state (eq (buffer-type buf) :dm))
               (let ((conn (app-first-connection app)))
                 (when conn
                   (handler-case
                       (xmpp-fetch-omemo-devicelist conn (buffer-name buf))
                     (error (e)
                       (debug-log "Fetch device list error: ~a" e))))))))))
      ((string= cmd "/help")
       (let ((buf (app-current-buffer app)))
         (when buf
           (buffer-add-message buf
             (make-message :text "Commands: /join <room> /msg <jid> /omemo /quit /help"
                           :level :system))))))))

(defun app-send-message (app text)
  "Send a message to the current buffer's XMPP connection."
  (let ((buf (app-current-buffer app)))
    (when (and buf (not (eq (buffer-type buf) :system)))
      (let* ((jid (buffer-name buf))
             (conn (app-first-connection app))
             (use-omemo (and (buffer-omemo-p buf)
                             (eq (buffer-type buf) :dm)
                             *omemo-device-id*)))
        (debug-log "Send: to=~a type=~a omemo=~a text=~a" jid (buffer-type buf) use-omemo text)
        (when conn
          (handler-case
              (cond
                ((eq (buffer-type buf) :muc)
                 (xmpp-send-groupchat conn jid text))
                ;; OMEMO-encrypted DM
                (use-omemo
                 (let ((encrypted-el (app-build-omemo-encrypted jid text)))
                   (if encrypted-el
                       (xmpp-send-omemo-message conn jid encrypted-el)
                       (progn
                         (debug-log "OMEMO encrypt failed, sending plaintext")
                         (xmpp-send-message conn jid text)))))
                ;; Plain DM
                ((eq (buffer-type buf) :dm)
                 (xmpp-send-message conn jid text)))
            (error (e)
              (debug-log "Send error: ~a" e)
              (buffer-add-message buf
                (make-message :text (format nil "Send error: ~A" e) :level :error)))))
        ;; Add to local buffer (for DMs only; MUC messages echo back from server)
        (when (eq (buffer-type buf) :dm)
          (let ((nick (if conn
                          (let ((bound (conn-bound-jid conn)))
                            (if bound (parse-jid bound) "me"))
                          "me")))
            (buffer-add-message buf
              (make-message :text text :nick nick))))))))

;;; ============================================================
;;; Splash screen flow
;;; ============================================================

(defun app-show-splash (app)
  "Show the startup splash screen."
  (let ((splash (make-instance 'splash-screen)))
    (setf (app-splash app) splash)
    (render-splash splash (app-width app) (app-height app))
    splash))

(defun app-splash-step (app step-name &key error-msg)
  "Advance the splash screen, optionally marking an error."
  (let ((splash (app-splash app)))
    (when splash
      (if error-msg
          (splash-error splash error-msg)
          (splash-advance splash))
      (render-splash splash (app-width app) (app-height app)))))

;;; ============================================================
;;; XMPP Connection
;;; ============================================================

(defun app-resolve-password (acct)
  "Resolve the password for an account config."
  (let ((pw (account-password acct))
        (jid (account-jid acct)))
    (cond
      ((eq pw :authinfo) (lookup-authinfo-password jid))
      ((stringp pw) pw)
      (t nil))))

(defvar *resolved-passwords* (make-hash-table :test 'equal)
  "Pre-resolved passwords keyed by JID (resolved before raw terminal mode).")

(defun app-resolve-passwords (app)
  "Resolve all account passwords before entering raw terminal mode.
   GPG pinentry needs a normal terminal to prompt for passphrase."
  (let ((cfg (app-config app)))
    (when cfg
      (dolist (acct (config-accounts cfg))
        (let ((jid (account-jid acct)))
          (format t "Resolving credentials for ~A...~%" jid)
          (force-output)
          (let ((pw (app-resolve-password acct)))
            (when pw
              (setf (gethash jid *resolved-passwords*) pw)
              (format t "  OK~%")
              (force-output))))))))

(defun app-connect-account (app acct)
  "Connect a single account. Returns the connection or NIL on error."
  (let* ((jid (account-jid acct))
         (password (or (gethash jid *resolved-passwords*)
                       (app-resolve-password acct)))
         (host (account-server acct))
         (port (account-port acct)))
    (unless password
      (error "No password found for ~A" jid))
    (let ((conn (xmpp-connect jid password
                              :host host :port port
                              :mechanism :plain)))
      (setf (gethash jid (app-connections app)) conn)
      conn)))

(defun app-start-receive-thread (app conn)
  "Start a background thread to receive XMPP stanzas."
  (setf (app-receive-thread app)
        (bt:make-thread
         (lambda ()
           (open-debug-log)
           (debug-log "=== CLabber receive thread started ===")
           (debug-log "conn-connected-p: ~a" (conn-connected-p conn))
           (debug-log "app-running-p: ~a" (app-running-p app))
           (debug-log "conn-stream: ~a" (conn-stream conn))
           (handler-case
               (loop while (and (app-running-p app)
                                (conn-connected-p conn))
                     do (handler-case
                            (let ((stanza (xmpp-receive conn)))
                              (when stanza
                                (debug-log "Stanza: ~a" (type-of stanza))
                                (app-push-event app (list :stanza stanza))))
                          (error (e)
                            (debug-log "Receive error: ~a" e)
                            (sleep 0.5))))
             (error (e)
               (debug-log "Receive thread fatal: ~a" e)))
           (debug-log "Receive thread exiting (running=~a connected=~a)"
                      (app-running-p app) (conn-connected-p conn))
           (app-push-event app (list :disconnected))
           (close-debug-log))
         :name "clabber-receive")))

(defun app-process-events (app)
  "Process all pending events from the receive thread."
  (let ((events (app-drain-events app)))
    (dolist (evt events)
      (handler-case
          (let ((type (first evt)))
            (case type
              (:stanza (app-handle-stanza app (second evt)))
              (:disconnected
               (let ((sys-buf (app-current-buffer app)))
                 (when sys-buf
                   (buffer-add-message sys-buf
                     (make-message :text "Disconnected from server" :level :error)))))))
        (error (e)
          (debug-log "Event processing error: ~a" e))))))

(defun app-handle-stanza (app stanza)
  "Dispatch a received stanza to the appropriate buffer."
  (typecase stanza
    (message-stanza
     (let* ((from (stanza-from stanza))
            (body (message-body stanza))
            (msg-type (stanza-type stanza))
            (delay (message-delay stanza))
            (omemo-el (message-omemo-encrypted stanza)))
       ;; Handle OMEMO device list PEP events
       (when (and (stanza-xml stanza)
                  (xml-child (stanza-xml stanza) "event"))
         (app-handle-omemo-devicelist app stanza))
       ;; Try OMEMO decryption if encrypted element present
       (let ((decrypted-body (when omemo-el (app-try-decrypt-omemo app stanza))))
         (let ((display-body (or decrypted-body body))
               (is-encrypted (not (null decrypted-body))))
           (when (and from display-body (> (length display-body) 0))
             (let* ((is-muc (and msg-type (string= msg-type "groupchat")))
                    (buf-name (bare-jid from))
                    (nick (if is-muc (jid-resource from) (bare-jid from)))
                    (buf (app-find-or-create-buffer app buf-name
                           :type (if is-muc :muc :dm)
                           :display-name (when is-muc (strip-muc-name buf-name)))))
               ;; Mark buffer as OMEMO-capable if we decrypted successfully
               (when is-encrypted
                 (setf (buffer-omemo-p buf) t))
               (buffer-add-message buf
                 (make-message :text display-body :nick nick
                               :timestamp (if delay
                                              (or (ignore-errors (parse-xmpp-timestamp delay))
                                                  (get-universal-time))
                                              (get-universal-time))))))
           ;; If OMEMO message but we couldn't decrypt, show indicator
           ;; (but not for key-transport messages which have no payload)
           (when (and omemo-el (not decrypted-body) from
                      ;; Only show if there was a payload (not key-transport)
                      (let ((header (xml-child omemo-el "header")))
                        (xml-child omemo-el "payload")))
             (let* ((is-muc (and msg-type (string= msg-type "groupchat")))
                    (buf-name (bare-jid from))
                    (buf (app-find-or-create-buffer app buf-name
                           :type (if is-muc :muc :dm)
                           :display-name (when is-muc (strip-muc-name buf-name)))))
               (buffer-add-message buf
                 (make-message :text "🔒 [OMEMO encrypted message - unable to decrypt]"
                               :nick (if is-muc (jid-resource from) (bare-jid from))
                               :level :system
                               :timestamp (if delay
                                              (or (ignore-errors (parse-xmpp-timestamp delay))
                                                  (get-universal-time))
                                              (get-universal-time))))))))))
    (presence-stanza
     (let* ((from (stanza-from stanza))
            (type- (stanza-type stanza))
            (show (presence-show stanza))
            (resource (when from (jid-resource from)))
            (bare (when from (bare-jid from))))
       (when from
         (cond
           ;; Auto-accept subscription
           ((and type- (string= type- "subscribe"))
            (let ((conn (app-first-connection app)))
              (when conn
                (xmpp-accept-subscription conn bare)
                (xmpp-send-presence conn :to bare :type "subscribe"))))
           ;; MUC presence — only update existing MUC buffers (created by bookmarks)
           (resource
            (let ((buf (app-find-buffer app bare)))
              (cond
                ;; Known MUC buffer: update participants
                ((and buf (eq (buffer-type buf) :muc))
                 (let ((presence-str (or show
                                         (if (and type- (string= type- "unavailable"))
                                             "offline" "available"))))
                   (if (string= presence-str "offline")
                       (setf (buffer-participants buf)
                             (remove resource (buffer-participants buf) :test #'string=))
                       (unless (member resource (buffer-participants buf) :test #'string=)
                         (push resource (buffer-participants buf))))))
                ;; Known DM buffer or no buffer: update contact presence
                (t
                 (let ((contact (gethash bare (app-contacts app))))
                   (when contact
                     (setf (contact-presence contact)
                           (cond
                             ((and type- (string= type- "unavailable")) :offline)
                             ((null show) :online)
                             ((string= show "away") :away)
                             ((string= show "dnd") :dnd)
                             ((string= show "xa") :xa)
                             (t :online)))))))))
           ;; Regular contact presence (no resource)
           (t
            (let ((contact (gethash bare (app-contacts app))))
              (unless contact
                (setf contact (make-contact :jid bare))
                (setf (gethash bare (app-contacts app)) contact))
              (setf (contact-presence contact)
                    (cond
                      ((and type- (string= type- "unavailable")) :offline)
                      ((null show) :online)
                      ((string= show "away") :away)
                      ((string= show "dnd") :dnd)
                      ((string= show "xa") :xa)
                      (t :online)))))))))
    (iq-stanza
     (let ((type- (stanza-type stanza))
           (query (iq-query stanza))
           (conn (app-first-connection app)))
       (debug-log "IQ: type=~a id=~a query=~a query-type=~a"
                  type- (stanza-id stanza)
                  (when (typep query 'xml-element) (xml-name query))
                  (when (typep query 'xml-element) (xml-namespace query)))
       (when (and type- query (typep query 'xml-element))
         ;; Handle roster result
         (when (and (string= type- "result")
                    (string= (xml-name query) "query")
                    (or (string= (or (xml-namespace query) "") +ns-roster+)
                        (search "roster" (or (xml-namespace query) ""))))
           (let ((count 0))
             (dolist (child (xml-children query))
               (when (and (typep child 'xml-element)
                          (string= (xml-name child) "item"))
                 (let ((jid (xml-attr child "jid"))
                       (name (xml-attr child "name")))
                   (when jid
                     (incf count)
                     (setf (gethash jid (app-contacts app))
                           (make-contact :jid jid :nick name))
                     ;; Create DM buffer for each roster contact
                     (app-find-or-create-buffer app jid
                       :type :dm :display-name (or name jid))))))
             (debug-log "Roster: ~a contacts loaded" count)))
         ;; Handle roster push (type=set)
         (when (and (string= type- "set")
                    (string= (xml-name query) "query")
                    (or (string= (or (xml-namespace query) "") +ns-roster+)
                        (search "roster" (or (xml-namespace query) ""))))
           ;; Acknowledge
           (when (and conn (stanza-id stanza))
             (handler-case
                 (xmpp-stream-send (conn-stream conn)
                                   (make-xml-element "iq"
                                     :attributes `(("type" . "result")
                                                   ("id" . ,(stanza-id stanza)))))
               (error () nil))))
         ;; Handle bookmarks
         (when (and (string= type- "result") (not (app-bookmarks-received-p app)))
           (let ((bookmarks (parse-bookmarks stanza)))
             (debug-log "Bookmarks parse: ~a results" (length bookmarks))
             (when bookmarks
               (setf (app-bookmarks-received-p app) t)
               (dolist (bm bookmarks)
                 (let ((jid (getf bm :jid))
                       (autojoin (getf bm :autojoin)))
                   (debug-log "Bookmark: ~a autojoin=~a" jid autojoin)
                   (when (and jid autojoin conn)
                     (handler-case
                         (progn
                           (debug-log "Joining MUC: ~a" jid)
                           (xmpp-join-muc conn jid)
                           (app-find-or-create-buffer app jid
                             :type :muc
                             :display-name (strip-muc-name jid))
                           (xmpp-query-mam conn jid :max 50))
                       (error (e)
                         (debug-log "MUC join error for ~a: ~a" jid e)))))))))
         ;; Handle PubSub/OMEMO device list and bundle responses
         (when (and (string= type- "result")
                    (string= (xml-name query) "pubsub"))
           (let ((items-el (xml-child query "items")))
             (when items-el
               (let ((node (or (xml-attr items-el "node") "")))
                 (debug-log "PubSub items node=~a" node)
                 (cond
                   ;; Device list response
                   ((search "devicelist" node)
                    (let ((device-ids nil))
                      (labels ((find-devices (el)
                                 (when (typep el 'xml-element)
                                   (cond
                                     ((string= (xml-name el) "device")
                                      (let ((id (xml-attr el "id")))
                                        (when id (push (parse-integer id) device-ids))))
                                     (t (dolist (c (xml-children el))
                                          (find-devices c)))))))
                        (find-devices items-el))
                      (when device-ids
                        (let ((from-jid (or (when (stanza-from stanza) (bare-jid (stanza-from stanza)))
                                            (when conn (bare-jid (conn-bound-jid conn))))))
                          (when from-jid
                            (cache-device-list from-jid device-ids)
                            (debug-log "PubSub: cached device list for ~a: ~a" from-jid device-ids)
                            ;; Auto-fetch bundles for each device
                            (when conn
                              (dolist (did device-ids)
                                (unless (signal-has-session-p from-jid did)
                                  (debug-log "Fetching bundle for ~a/~a" from-jid did)
                                  (handler-case
                                      (xmpp-fetch-omemo-bundle conn from-jid did)
                                    (error (e) (debug-log "Bundle fetch error: ~a" e)))))))))))
                   ;; Bundle response
                   ((search "bundles:" node)
                    (let* ((device-id-str (subseq node (1+ (position #\: node))))
                           (device-id (parse-integer device-id-str))
                           (from-jid (or (when (stanza-from stanza) (bare-jid (stanza-from stanza)))
                                         (when conn (bare-jid (conn-bound-jid conn))))))
                      (debug-log "PubSub: bundle response for ~a/~a" from-jid device-id)
                      (when from-jid
                        (app-process-omemo-bundle app from-jid device-id items-el)))))))))
         ;; Handle MAM results (messages wrapped in <result> inside <message>)
         )))
    (t nil)))

(defun app-find-buffer (app name)
  "Find a buffer by name."
  (loop for i from 0 below (fill-pointer (app-buffers app))
        for buf = (aref (app-buffers app) i)
        when (string= (buffer-name buf) name)
        return buf))

(defun app-find-or-create-buffer (app name &key (type :dm) display-name)
  "Find or create a buffer by name."
  (or (app-find-buffer app name)
      (let ((buf (make-buffer :name name
                              :display-name (or display-name
                                                (when (eq type :muc) (strip-muc-name name))
                                                name)
                              :type type)))
        (app-add-buffer app buf)
        buf)))

(defun parse-xmpp-timestamp (stamp)
  "Parse an ISO 8601 timestamp to universal time."
  (let ((year (parse-integer (subseq stamp 0 4)))
        (month (parse-integer (subseq stamp 5 7)))
        (day (parse-integer (subseq stamp 8 10)))
        (hour (parse-integer (subseq stamp 11 13)))
        (min (parse-integer (subseq stamp 14 16)))
        (sec (parse-integer (subseq stamp 17 19))))
    (encode-universal-time sec min hour day month year 0)))

;;; ============================================================
;;; OMEMO helpers
;;; ============================================================

(defun omemo-bundle-xml-element ()
  "Build OMEMO bundle as an xml-element for PEP publishing.
   Uses signal-get-identity-public, signal-generate-prekeys, signal-generate-signed-prekey
   which are already called during omemo-init."
  (handler-case
      (let* ((identity-pub (signal-get-identity-public))
             (prekeys (signal-generate-prekeys 1 100)))
        (debug-log "Bundle: identity-pub=~a bytes, prekeys=~a"
                   (length identity-pub) (length prekeys))
        (multiple-value-bind (spk-pub spk-sig)
            (signal-generate-signed-prekey 1)
          (debug-log "Bundle: spk-pub=~a bytes, spk-sig=~a bytes"
                     (when spk-pub (length spk-pub))
                     (when spk-sig (length spk-sig)))
          (make-xml-element "bundle"
            :namespace +omemo-ns-legacy+
            :children
            (list
             (make-xml-element "signedPreKeyPublic"
               :attributes '(("signedPreKeyId" . "1"))
               :text (bytes-to-base64 spk-pub))
             (make-xml-element "signedPreKeySignature"
               :text (bytes-to-base64 spk-sig))
             (make-xml-element "identityKey"
               :text (bytes-to-base64 identity-pub))
             (make-xml-element "prekeys"
               :children (loop for pk in prekeys
                               collect (make-xml-element "preKeyPublic"
                                         :attributes `(("preKeyId" . ,(princ-to-string (car pk))))
                                         :text (bytes-to-base64 (cdr pk)))))))))
    (error (e)
      (debug-log "omemo-bundle-xml-element error: ~a" e)
      nil)))

(defun app-build-omemo-encrypted (to-jid plaintext)
  "Build an OMEMO <encrypted> xml-element for sending to TO-JID.
   Returns xml-element or NIL on failure."
  (handler-case
      (let ((device-ids (get-cached-device-list to-jid)))
        (unless device-ids
          (debug-log "No OMEMO devices known for ~a" to-jid)
          (return-from app-build-omemo-encrypted nil))
        (multiple-value-bind (iv ciphertext key-material)
            (omemo-encrypt-payload plaintext)
          (let ((key-elements nil))
            ;; Encrypt key-material for each recipient device
            (dolist (device-id device-ids)
              (handler-case
                  (multiple-value-bind (encrypted-key msg-type)
                      (signal-session-encrypt to-jid device-id key-material)
                    (push (make-xml-element "key"
                            :attributes `(("rid" . ,(princ-to-string device-id))
                                          ,@(when (= msg-type +ciphertext-prekey-type+)
                                              '(("prekey" . "true"))))
                            :text (bytes-to-base64 encrypted-key))
                          key-elements))
                (error (e)
                  (debug-log "OMEMO: can't encrypt to ~a/~a: ~a" to-jid device-id e))))
            (unless key-elements
              (debug-log "OMEMO: no devices could be encrypted to")
              (return-from app-build-omemo-encrypted nil))
            ;; Build the <encrypted> element
            (make-xml-element "encrypted"
              :namespace +omemo-ns-legacy+
              :children
              (list
               (make-xml-element "header"
                 :attributes `(("sid" . ,(princ-to-string *omemo-device-id*)))
                 :children (append (nreverse key-elements)
                                   (list (make-xml-element "iv"
                                           :text (bytes-to-base64 iv)))))
               (make-xml-element "payload"
                 :text (bytes-to-base64 ciphertext)))))))
    (error (e)
      (debug-log "app-build-omemo-encrypted error: ~a" e)
      nil)))

(defun app-try-decrypt-omemo (app stanza)
  "Try to decrypt an OMEMO-encrypted message stanza.
   Returns decrypted plaintext string or NIL."
  (declare (ignore app))
  (let ((encrypted-el (message-omemo-encrypted stanza)))
    (when encrypted-el
      (handler-case
          (let* ((header (xml-child encrypted-el "header"))
                 (payload-el (xml-child encrypted-el "payload"))
                 (sender-sid (when header (xml-attr header "sid")))
                 (iv-el (when header (xml-child header "iv")))
                 (our-device-id *omemo-device-id*))
            (unless (and header payload-el iv-el our-device-id)
              ;; Key-transport messages (no payload) are normal - don't log
              (return-from app-try-decrypt-omemo nil))
            ;; Log all key rids to find ours
            (let ((all-rids nil))
              (dolist (child (xml-children header))
                (when (and (typep child 'xml-element) (string= (xml-name child) "key"))
                  (push (xml-attr child "rid") all-rids)))
              (debug-log "OMEMO decrypt: key rids=~a looking-for=~a" (nreverse all-rids) our-device-id))
            ;; Find the <key> element addressed to our device
            (let ((our-key-el nil))
              (dolist (child (xml-children header))
                (when (and (typep child 'xml-element)
                           (string= (xml-name child) "key")
                           (string= (or (xml-attr child "rid") "") (princ-to-string our-device-id)))
                  (setf our-key-el child)))
              (unless our-key-el
                (debug-log "OMEMO decrypt: no key element for our device ~a" our-device-id)
                (return-from app-try-decrypt-omemo nil))
              (let* ((key-data (base64-to-bytes (xml-text our-key-el)))
                     (is-prekey (or (string= (or (xml-attr our-key-el "prekey") "") "true")
                                    (string= (or (xml-attr our-key-el "prekey") "") "1")))
                     (iv (base64-to-bytes (xml-text iv-el)))
                     (ciphertext (base64-to-bytes (xml-text payload-el)))
                     (from-jid (bare-jid (stanza-from stanza)))
                     (sender-device-id (parse-integer sender-sid)))
                (debug-log "OMEMO decrypt: from=~a sid=~a prekey=~a key-len=~a iv-len=~a ct-len=~a"
                           from-jid sender-device-id is-prekey
                           (length key-data) (length iv) (length ciphertext))
                (omemo-decrypt-message from-jid sender-device-id
                                       key-data is-prekey iv ciphertext))))
        (error (e)
          (debug-log "OMEMO decrypt error: ~a" e)
          nil)))))

(defun app-process-omemo-bundle (app jid device-id items-el)
  "Parse an OMEMO bundle from PubSub items and build a Signal session."
  (declare (ignore app))
  (handler-case
      (let ((bundle-el nil))
        ;; Find the <bundle> element inside <items><item><bundle>
        (labels ((find-bundle (el)
                   (when (typep el 'xml-element)
                     (if (string= (xml-name el) "bundle")
                         (setf bundle-el el)
                         (dolist (c (xml-children el))
                           (find-bundle c))))))
          (find-bundle items-el))
        (unless bundle-el
          (debug-log "Bundle parse: no <bundle> element found")
          (return-from app-process-omemo-bundle nil))
        (let* ((spk-pub-el (xml-child bundle-el "signedPreKeyPublic"))
               (spk-sig-el (xml-child bundle-el "signedPreKeySignature"))
               (ik-el (xml-child bundle-el "identityKey"))
               (prekeys-el (xml-child bundle-el "prekeys")))
          (unless (and spk-pub-el spk-sig-el ik-el)
            (debug-log "Bundle parse: missing required elements")
            (return-from app-process-omemo-bundle nil))
          (let* ((spk-id (parse-integer (or (xml-attr spk-pub-el "signedPreKeyId") "1")))
                 (spk-pub (base64-to-bytes (xml-text spk-pub-el)))
                 (spk-sig (base64-to-bytes (xml-text spk-sig-el)))
                 (ik (base64-to-bytes (xml-text ik-el)))
                 ;; Pick a random prekey
                 (prekey-els (when prekeys-el
                               (remove-if-not
                                (lambda (c) (and (typep c 'xml-element)
                                                 (string= (xml-name c) "preKeyPublic")))
                                (xml-children prekeys-el))))
                 (chosen-pk (when prekey-els
                              (nth (random (length prekey-els)) prekey-els)))
                 (pk-id (when chosen-pk
                          (parse-integer (or (xml-attr chosen-pk "preKeyId") "1"))))
                 (pk-pub (when chosen-pk
                           (base64-to-bytes (xml-text chosen-pk)))))
            (debug-log "Bundle parse: spk-id=~a spk-pub=~a ik=~a pk-id=~a pk-pub=~a"
                       spk-id (length spk-pub) (length ik) pk-id
                       (when pk-pub (length pk-pub)))
            (when (and pk-pub spk-pub spk-sig ik)
              (signal-build-session jid device-id
                                    0  ; registration-id (not needed for session building)
                                    pk-id pk-pub
                                    spk-id spk-pub
                                    spk-sig ik)
              (debug-log "Session built for ~a/~a" jid device-id)))))
    (error (e)
      (debug-log "Bundle process error for ~a/~a: ~a" jid device-id e))))

(defun app-handle-omemo-devicelist (app stanza)
  "Handle incoming OMEMO device list PEP event."
  (declare (ignore app))
  (let* ((from (stanza-from stanza))
         (jid (when from (bare-jid from))))
    (when jid
      (handler-case
          (let ((items-el (xml-child (stanza-xml stanza) "event")))
            (when items-el
              (labels ((find-devices (el)
                         (when (typep el 'xml-element)
                           (cond
                             ((string= (xml-name el) "device")
                              (let ((id (xml-attr el "id")))
                                (when id (list (parse-integer id)))))
                             (t (mapcan #'find-devices (xml-children el)))))))
                (let ((device-ids (find-devices items-el)))
                  (when device-ids
                    (cache-device-list jid device-ids)
                    (debug-log "Cached device list for ~a: ~a" jid device-ids))))))
        (error (e)
          (debug-log "Device list parse error: ~a" e))))))

;;; ============================================================
;;; Main event loop
;;; ============================================================

(defun app-run (app)
  "Main application loop."
  (with-raw-terminal
    ;; Get terminal size
    (let ((size (terminal-size)))
      (setf (app-width app) (first size)
            (app-height app) (second size)))

    ;; Open debug log early so connect/OMEMO init gets logged
    (open-debug-log)

    ;; Show splash
    (app-show-splash app)

    ;; Connection phase
    (let ((cfg (app-config app))
          (conn nil)
          (connect-error nil))
      ;; Step 1: Connecting
      (app-splash-step app "Connecting to server")
      (let ((accounts (when cfg (config-accounts cfg))))
        (if (null accounts)
            (progn
              (app-splash-step app "No accounts" :error-msg "No accounts configured")
              (setf connect-error t))
            (handler-case
                (let ((acct (first accounts)))
                  (setf conn (app-connect-account app acct)))
              (error (e)
                (app-splash-step app "Connection failed"
                                 :error-msg (format nil "~A" e))
                (setf connect-error t)))))

      (unless connect-error
        ;; Step 2: Authenticated (already done by xmpp-connect)
        (app-splash-step app "Authenticating")

        ;; Step 3: Send initial presence & request roster
        (app-splash-step app "Loading roster")
        (handler-case
            (progn
              (xmpp-send-presence conn)
              (xmpp-get-roster conn)
              (xmpp-get-bookmarks conn)
              (handler-case (xmpp-enable-carbons conn) (error () nil)))
          (error () nil))

        ;; Step 4: Joining rooms (will happen async via bookmarks)
        (app-splash-step app "Joining rooms")

        ;; Step 5: Initialize OMEMO
        (app-splash-step app "Initializing OMEMO")
        (handler-case
            (progn
              (omemo-init)
              (debug-log "OMEMO initialized, device-id=~a" *omemo-device-id*)
              ;; Publish device list and bundle
              (when *omemo-device-id*
                (xmpp-publish-omemo-devicelist conn *omemo-device-id*)
                (let ((bundle-xml (omemo-bundle-xml-element)))
                  (when bundle-xml
                    (xmpp-publish-omemo-bundle conn *omemo-device-id* bundle-xml)))
                (debug-log "OMEMO device list and bundle published")))
          (error (e)
            (debug-log "OMEMO init error (non-fatal): ~a" e)))

        ;; Mark running BEFORE starting receive thread
        (setf (app-running-p app) t)

        ;; Start receive thread
        (app-start-receive-thread app conn))

      (sleep 0.3))

    ;; Create system buffer
    (app-add-buffer app (make-buffer :name "*system*" :display-name "*system*" :type :system))
    (buffer-add-message (app-current-buffer app)
      (make-message :text (format nil "Welcome to CLabber ~A" *version*)
                    :level :system))
    (when (app-first-connection app)
      (buffer-add-message (app-current-buffer app)
        (make-message :text (format nil "Connected as ~A"
                                    (conn-bound-jid (app-first-connection app)))
                      :level :system))
      (when *omemo-device-id*
        (buffer-add-message (app-current-buffer app)
          (make-message :text (format nil "OMEMO device ID: ~A (type /omemo in a DM to enable)"
                                      *omemo-device-id*)
                        :level :system))))

    ;; Compute layout and render
    (app-update-layout app)
    (unless (app-running-p app) (setf (app-running-p app) t))

    ;; Ensure debug log is open for main thread too
    (open-debug-log)

    ;; Main loop
    (unwind-protect
         (loop while (app-running-p app) do
           ;; Process XMPP events from receive thread
           (handler-case (app-process-events app)
             (error (e) (debug-log "Main loop event error: ~a" e)))
           ;; Handle keyboard input
           (let ((key (read-key-with-timeout 33))) ; ~30fps
             (when key
               (unless (app-handle-input app key)
                 (setf (app-running-p app) nil))))
           ;; Re-render (catch errors to avoid spewing to terminal)
           (handler-case (app-render app)
             (error (e)
               (debug-log "Render error: ~a" e))))
      ;; Cleanup
      (setf (app-running-p app) nil)
      (let ((conn (app-first-connection app)))
        (when conn
          (handler-case (xmpp-disconnect conn) (error () nil)))))))
