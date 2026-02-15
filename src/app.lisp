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
                  :documentation "Position after the last inserted completion")
   ;; Chat state notification tracking
   (composing-sent-p :initform nil :accessor app-composing-sent-p
                     :documentation "Whether we've sent <composing> for current input")
   (typing-indicators :initform (make-hash-table :test 'equal)
                      :accessor app-typing-indicators
                      :documentation "JID -> (nick . timestamp) of who is typing")
   ;; Focus mode for panel navigation
   (focus-mode :initform :input :accessor app-focus-mode
               :documentation "Current focus: :input or :participants")
   ;; Emoji picker state
   (emoji-active-p :initform nil :accessor app-emoji-active-p
                   :documentation "Whether the emoji picker popup is showing")
   (emoji-prefix :initform "" :accessor app-emoji-prefix
                 :documentation "Current search prefix after :")
   (emoji-matches :initform nil :accessor app-emoji-matches
                  :documentation "List of (code . emoji) matching current prefix")
   (emoji-index :initform 0 :accessor app-emoji-index
                :documentation "Selected index in emoji matches")
   (emoji-colon-pos :initform 0 :accessor app-emoji-colon-pos
                    :documentation "Position of the : that started emoji search")
   (last-ping-time :initform 0 :accessor app-last-ping-time
                   :documentation "Universal time of last XMPP keepalive ping"))
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

(defun app-ordered-buffers (app)
  "Return buffers in display order: system, DMs, MUCs."
  (append
   (loop for i from 0 below (fill-pointer (app-buffers app))
         for b = (aref (app-buffers app) i)
         when (eq (buffer-type b) :system) collect b)
   (app-dm-buffers app)
   (app-muc-buffers app)))

(defun app-switch-buffer (app direction)
  "Switch to next/previous buffer in display order. DIRECTION is :next or :prev."
  (let* ((ordered (app-ordered-buffers app))
         (count (length ordered))
         (cur (app-current-buffer app))
         (cur-idx (or (position cur ordered) 0))
         (new-idx (mod (+ cur-idx (if (eq direction :next) 1 -1)) count))
         (new-buf (nth new-idx ordered)))
    (when new-buf
      (app-switch-to-buffer app new-buf))))

(defun app-switch-to-buffer (app buf)
  "Switch active buffer to BUF."
  (loop for i from 0 below (fill-pointer (app-buffers app))
        when (eq (aref (app-buffers app) i) buf)
        do (setf (app-active-buffer app) i
                 (buffer-unread-count buf) 0
                 (buffer-mention-p buf) nil)
           (return)))

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
    ;; Active buffer is always "read"
    (when buf
      (setf (buffer-unread-count buf) 0
            (buffer-mention-p buf) nil))
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
      ;; Show typing indicator or OMEMO status on the right
      (let* ((typing-text
               (when buf
                 (let ((indicator (gethash (buffer-name buf) (app-typing-indicators app))))
                   (when (and indicator
                              ;; Expire after 10 seconds
                              (< (- (get-universal-time) (cdr indicator)) 10))
                     (format nil "~a is typing... " (car indicator))))))
             (correct-text (if (and buf (buffer-correcting-p buf)) "✏️ editing " ""))
             (omemo-text (if (and buf (buffer-omemo-p buf)) "🔒 OMEMO " "")))
        (setf (status-bar-right (layout-status ly))
              (concatenate 'string
                (or typing-text "")
                correct-text
                omemo-text)))) ;; closes concatenate, setf, let*, when
    ;; Update buffer bar (ordered: system, DMs, MUCs to match roster)
    (when (layout-buffer-bar ly)
      (let* ((ordered (app-ordered-buffers app))
             (active-buf (app-current-buffer app))
             (bar-idx (or (position active-buf ordered) 0)))
        (setf (buffer-bar-buffers (layout-buffer-bar ly)) ordered)
        (setf (buffer-bar-active-index (layout-buffer-bar ly)) bar-idx)))
    ;; Render all (with emoji popup inside the sync update)
    (begin-sync-update)
    (when (layout-roster ly) (panel-render (layout-roster ly)))
    (when (layout-chat-a ly) (panel-render (layout-chat-a ly)))
    (when (layout-chat-b ly) (panel-render (layout-chat-b ly)))
    (when (layout-participants ly) (panel-render (layout-participants ly)))
    (when (layout-buffer-bar ly) (panel-render (layout-buffer-bar ly)))
    (when (layout-status ly) (panel-render (layout-status ly)))
    (when (layout-input ly) (panel-render (layout-input ly)))
    ;; Render emoji picker popup inside sync update so it doesn't flicker
    (when (app-emoji-active-p app)
      (let* ((matches (app-emoji-matches app))
             (max-show (min 8 (length matches)))
             (input (layout-input ly))
             (popup-w 30)
             (popup-h (+ max-show 2))
             (popup-x (max 1 (panel-x input)))
             (popup-y (max 1 (- (panel-y input) popup-h)))
             (theme (current-theme)))
        (when (> max-show 0)
          ;; Draw popup background
          (loop for row from popup-y below (+ popup-y popup-h)
                do (cursor-to row popup-x)
                   (emit-bg (theme-bg theme) *terminal-io*)
                   (emit-fg (theme-border-active theme) *terminal-io*)
                   (princ (make-string popup-w :initial-element #\Space) *terminal-io*))
          ;; Top border
          (cursor-to popup-y popup-x)
          (emit-fg (theme-border-active theme) *terminal-io*)
          (princ (concatenate 'string "┌" (make-string (- popup-w 2) :initial-element #\─) "┐")
                 *terminal-io*)
          ;; Bottom border
          (cursor-to (+ popup-y popup-h -1) popup-x)
          (princ (concatenate 'string "└" (make-string (- popup-w 2) :initial-element #\─) "┘")
                 *terminal-io*)
          ;; Emoji entries
          (loop for i from 0 below max-show
                for entry in matches
                for row = (+ popup-y 1 i)
                do (cursor-to row popup-x)
                   (emit-bg (theme-bg theme) *terminal-io*)
                   (emit-fg (theme-border-active theme) *terminal-io*)
                   (princ "│" *terminal-io*)
                   (if (= i (app-emoji-index app))
                       (progn (inverse) (bold))
                       (emit-fg (theme-fg theme) *terminal-io*))
                   (let* ((code (car entry))
                          (emoji (cdr entry))
                          (label (format nil " ~A :~A:" emoji code))
                          (max-label (- popup-w 2))
                          (display (if (> (length label) max-label)
                                       (subseq label 0 max-label)
                                       (concatenate 'string label
                                         (make-string (- max-label (length label))
                                                      :initial-element #\Space)))))
                     (princ display *terminal-io*))
                   (reset)
                   (emit-fg (theme-border-active theme) *terminal-io*)
                   (emit-bg (theme-bg theme) *terminal-io*)
                   (princ "│" *terminal-io*))
          (reset))))
    (end-sync-update)
    (force-output *terminal-io*)))

;;; ============================================================
;;; Input handling
;;; ============================================================

(defun app-handle-participants-input (app key)
  "Handle input when WHO panel is focused. Returns NIL to quit, T to continue."
  (let* ((ly (app-layout app))
         (panel (layout-participants ly)))
    (when panel
      (let ((count (length (participants-list panel)))
            (sel (participants-selected-index panel)))
        (cond
          ;; Escape or Ctrl-O: return to input mode
          ((or (eql (key-event-code key) +key-escape+)
               (and (key-event-ctrl-p key) (eql (key-event-char key) #\o)))
           (setf (participants-selected-index panel) -1
                 (app-focus-mode app) :input))
          ;; Up arrow: move selection up
          ((eql (key-event-code key) +key-up+)
           (when (> count 0)
             (setf (participants-selected-index panel)
                   (if (<= sel 0) (1- count) (1- sel)))))
          ;; Down arrow: move selection down
          ((eql (key-event-code key) +key-down+)
           (when (> count 0)
             (setf (participants-selected-index panel)
                   (if (>= sel (1- count)) 0 (1+ sel)))))
          ;; Enter: open DM with selected participant
          ((eql (key-event-code key) +key-enter+)
           (when (and (>= sel 0) (< sel count))
             (let* ((sorted (sort (copy-list (participants-list panel))
                                  (lambda (a b)
                                    (let ((ra (clabber.model:participant-role a))
                                          (rb (clabber.model:participant-role b)))
                                      (or (> (role-sort-key ra) (role-sort-key rb))
                                          (and (= (role-sort-key ra) (role-sort-key rb))
                                               (string< (clabber.model:participant-nick a)
                                                        (clabber.model:participant-nick b))))))))
                    (entry (nth sel sorted))
                    (nick (clabber.model:participant-nick entry))
                    (buf (app-current-buffer app)))
               (when (and buf nick)
                 ;; Build DM JID: for biboumi IRC, nick%server@gateway
                 (let* ((muc-jid (buffer-name buf))
                        (at-pos (position #\@ muc-jid))
                        (pct-pos (position #\% muc-jid))
                        (dm-jid (if (and at-pos pct-pos (< pct-pos at-pos))
                                    ;; Biboumi MUC: #chan%irc.server@gateway
                                    ;; PM JID: nick%irc.server@gateway
                                    (format nil "~a~a" nick (subseq muc-jid pct-pos))
                                    ;; Native XMPP MUC: use occupant JID
                                    (format nil "~a/~a" muc-jid nick)))
                        (dm-buf (app-find-or-create-buffer app dm-jid
                                  :type :dm :display-name nick)))
                   (app-switch-to-buffer app dm-buf)
                   (setf (participants-selected-index panel) -1
                         (app-focus-mode app) :input))))))
          ;; Ctrl-Q: quit even from participants mode
          ((and (key-event-ctrl-p key) (eql (key-event-char key) #\q))
           (return-from app-handle-participants-input nil))))))
  t)

(defun app-handle-input (app key)
  "Handle a key event. Returns NIL to quit, T to continue."
  ;; Dispatch to participants panel handler if focused
  (when (eq (app-focus-mode app) :participants)
    (return-from app-handle-input (app-handle-participants-input app key)))
  ;; Handle emoji picker input if active
  (when (app-emoji-active-p app)
    (let ((input (layout-input (app-layout app))))
      (cond
        ;; Escape: dismiss picker
        ((eql (key-event-code key) +key-escape+)
         (setf (app-emoji-active-p app) nil))
        ;; Up arrow: move selection up
        ((eql (key-event-code key) +key-up+)
         (when (> (length (app-emoji-matches app)) 0)
           (setf (app-emoji-index app)
                 (mod (1- (app-emoji-index app))
                      (min 8 (length (app-emoji-matches app)))))))
        ;; Down arrow: move selection down
        ((eql (key-event-code key) +key-down+)
         (when (> (length (app-emoji-matches app)) 0)
           (setf (app-emoji-index app)
                 (mod (1+ (app-emoji-index app))
                      (min 8 (length (app-emoji-matches app)))))))
        ;; Tab or Enter: insert selected emoji
        ((or (eql (key-event-code key) +key-tab+)
             (eql (key-event-code key) +key-enter+))
         (when (and (app-emoji-matches app)
                    (< (app-emoji-index app) (length (app-emoji-matches app))))
           (let* ((entry (nth (app-emoji-index app) (app-emoji-matches app)))
                  (emoji (cdr entry))
                  (text (input-bar-text input))
                  (colon-pos (app-emoji-colon-pos app))
                  (cursor (input-bar-cursor-pos input))
                  (new-text (concatenate 'string
                              (subseq text 0 colon-pos)
                              emoji
                              (subseq text cursor)))
                  (new-pos (+ colon-pos (length emoji))))
             (setf (input-bar-text input) new-text
                   (input-bar-cursor-pos input) new-pos)))
         (setf (app-emoji-active-p app) nil))
        ;; Backspace: remove char from prefix or dismiss if empty
        ((eql (key-event-code key) +key-backspace+)
         (let ((pos (input-bar-cursor-pos input))
               (text (input-bar-text input)))
           (when (> pos 0)
             (setf (input-bar-text input)
                   (concatenate 'string (subseq text 0 (1- pos)) (subseq text pos)))
             (decf (input-bar-cursor-pos input))
             ;; Update prefix or dismiss if we backspaced past the :
             (if (<= (input-bar-cursor-pos input) (app-emoji-colon-pos app))
                 (setf (app-emoji-active-p app) nil)
                 (let ((new-prefix (subseq (input-bar-text input)
                                           (1+ (app-emoji-colon-pos app))
                                           (input-bar-cursor-pos input))))
                   (setf (app-emoji-prefix app) new-prefix
                         (app-emoji-matches app) (emoji-complete new-prefix)
                         (app-emoji-index app) 0))))))
        ;; Regular character: add to prefix and update matches
        ((key-event-char key)
         (let ((ch (key-event-char key)))
           (when (and (graphic-char-p ch) (not (char= ch #\Space)))
             (let ((pos (input-bar-cursor-pos input))
                   (text (input-bar-text input)))
               (setf (input-bar-text input)
                     (concatenate 'string (subseq text 0 pos) (string ch) (subseq text pos)))
               (incf (input-bar-cursor-pos input))
               (let ((new-prefix (subseq (input-bar-text input)
                                         (1+ (app-emoji-colon-pos app))
                                         (input-bar-cursor-pos input))))
                 (setf (app-emoji-prefix app) new-prefix
                       (app-emoji-matches app) (emoji-complete new-prefix)
                       (app-emoji-index app) 0)
                 ;; Dismiss if no matches
                 (unless (app-emoji-matches app)
                   (setf (app-emoji-active-p app) nil)))))
           ;; Space dismisses picker
           (when (char= ch #\Space)
             (let ((pos (input-bar-cursor-pos input))
                   (text (input-bar-text input)))
               (setf (input-bar-text input)
                     (concatenate 'string (subseq text 0 pos) (string ch) (subseq text pos)))
               (incf (input-bar-cursor-pos input)))
             (setf (app-emoji-active-p app) nil)))))
      (return-from app-handle-input t)))
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
      ;; Ctrl-O: focus WHO panel for participant selection
      ((and (key-event-ctrl-p key) (eql (key-event-char key) #\o))
       (let* ((ly (app-layout app))
              (panel (layout-participants ly))
              (buf (app-current-buffer app)))
         (when (and panel buf (eq (buffer-type buf) :muc)
                    (participants-list panel)
                    (> (length (participants-list panel)) 0))
           (setf (participants-selected-index panel) 0
                 (app-focus-mode app) :participants)))
       t)
      ;; Escape: cancel correction mode
      ((eql (key-event-code key) +key-escape+)
       (let ((buf (app-current-buffer app))
             (input (layout-input ly)))
         (when (and buf input (buffer-correcting-p buf))
           (setf (buffer-correcting-p buf) nil
                 (input-bar-text input) ""
                 (input-bar-cursor-pos input) 0)))
       t)
      ;; Ctrl-W: cycle split mode
      ((and (key-event-ctrl-p key) (eql (key-event-char key) #\w))
       (let ((current (layout-split-mode (app-layout app))))
         (setf (layout-split-mode (app-layout app))
               (case current
                 ((nil) :horizontal)
                 (:horizontal :vertical)
                 (:vertical nil))))
       (app-update-layout app) t)
      ;; Up arrow: recall last sent message (if input empty) or previous buffer
      ((eql (key-event-code key) +key-up+)
       (let ((input (layout-input ly))
             (buf (app-current-buffer app)))
         (if (and input buf
                  (= (length (input-bar-text input)) 0)
                  (buffer-last-sent-text buf)
                  (not (buffer-correcting-p buf)))
             ;; Recall last sent message for correction
             (progn
               (setf (input-bar-text input) (buffer-last-sent-text buf)
                     (input-bar-cursor-pos input) (length (buffer-last-sent-text buf))
                     (buffer-correcting-p buf) t))
             ;; Normal: switch buffer
             (app-switch-buffer app :prev)))
       t)
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
                                         (buffer-participant-nicks buf))
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
                   (input-bar-cursor-pos input) 0)
             ;; Send <active> chat state after sending message
             (setf (app-composing-sent-p app) nil)
             (let ((buf (app-current-buffer app))
                   (conn (app-first-connection app)))
               (when (and buf conn (eq (buffer-type buf) :dm))
                 (handler-case
                     (xmpp-send-chat-state conn (buffer-name buf) "active")
                   (error () nil)))))))
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
             (incf (input-bar-cursor-pos input))
             ;; Activate emoji picker when : is typed
             (when (char= ch #\:)
               (let ((matches (emoji-complete "")))
                 (setf (app-emoji-active-p app) t
                       (app-emoji-prefix app) ""
                       (app-emoji-matches app) matches
                       (app-emoji-index app) 0
                       (app-emoji-colon-pos app) (1- (input-bar-cursor-pos input))))))
           ;; Send <composing> chat state on first keystroke
           (unless (app-composing-sent-p app)
             (let ((buf (app-current-buffer app))
                   (conn (app-first-connection app)))
               (when (and buf conn (eq (buffer-type buf) :dm))
                 (handler-case
                     (progn
                       (xmpp-send-chat-state conn (buffer-name buf) "composing")
                       (setf (app-composing-sent-p app) t))
                   (error () nil)))))))
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
         (let ((conn (app-first-connection app))
               (jid (string-trim " " args)))
           (when conn
             (handler-case
                 (progn
                   (xmpp-join-muc conn jid)
                   (let ((buf (app-find-or-create-buffer app jid
                                :type :muc
                                :display-name (strip-muc-name jid))))
                     (xmpp-query-mam conn jid :max 50)
                     (app-switch-to-buffer app buf)
                     (buffer-add-message buf
                       (make-message :text (format nil "Joined ~A" jid) :level :system))))
               (error (e)
                 (let ((buf (app-current-buffer app)))
                   (when buf
                     (buffer-add-message buf
                       (make-message :text (format nil "Join error: ~A" e) :level :error))))))))))
      ((string= cmd "/part")
       (let ((buf (app-current-buffer app))
             (conn (app-first-connection app)))
         (when (and buf conn (eq (buffer-type buf) :muc))
           (handler-case
               (progn
                 (xmpp-leave-muc conn (buffer-name buf))
                 (buffer-add-message buf
                   (make-message :text (format nil "Left ~A" (buffer-name buf)) :level :system)))
             (error (e)
               (buffer-add-message buf
                 (make-message :text (format nil "Part error: ~A" e) :level :error)))))))
      ((string= cmd "/add")
       (when args
         (let ((conn (app-first-connection app))
               (jid (string-trim " " args)))
           (when conn
             (handler-case
                 (progn
                   (xmpp-add-contact conn jid)
                   (setf (gethash jid (app-contacts app))
                         (make-contact :jid jid))
                   (let ((buf (app-find-or-create-buffer app jid
                                :type :dm :display-name jid)))
                     (app-switch-to-buffer app buf)
                     (buffer-add-message buf
                       (make-message :text (format nil "Added ~A to contacts" jid) :level :system))))
               (error (e)
                 (let ((buf (app-current-buffer app)))
                   (when buf
                     (buffer-add-message buf
                       (make-message :text (format nil "Add error: ~A" e) :level :error))))))))))
      ((string= cmd "/msg")
       (when args
         (let* ((space (position #\Space args))
                (jid (if space (subseq args 0 space) args))
                (msg (when space (string-trim " " (subseq args space)))))
           (let ((buf (app-find-or-create-buffer app jid :type :dm :display-name jid)))
             (app-switch-to-buffer app buf)
             (when (and msg (> (length msg) 0))
               (app-send-message app msg))))))
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
      ((string= cmd "/upload")
       (if (not args)
           (let ((buf (app-current-buffer app)))
             (when buf
               (buffer-add-message buf
                 (make-message :text "Usage: /upload <filepath>" :level :system))))
           (let ((file-path (string-trim " " args))
                 (buf (app-current-buffer app))
                 (conn (app-first-connection app)))
             (cond
               ((not (probe-file file-path))
                (when buf
                  (buffer-add-message buf
                    (make-message :text (format nil "File not found: ~a" file-path) :level :error))))
               ((not *upload-service-jid*)
                (when buf
                  (buffer-add-message buf
                    (make-message :text "No HTTP Upload service discovered. Server may not support XEP-0363."
                                  :level :error))))
               ((and buf conn)
                (let* ((truename (truename file-path))
                       (filename (file-namestring truename))
                       (size (with-open-file (f truename :element-type '(unsigned-byte 8))
                               (file-length f)))
                       (content-type (guess-content-type filename)))
                  (setf *pending-upload*
                        (list :file (namestring truename)
                              :buffer buf
                              :content-type content-type))
                  (buffer-add-message buf
                    (make-message :text (format nil "Uploading ~a (~d bytes)..." filename size)
                                  :level :system))
                  (handler-case
                      (xmpp-request-upload-slot conn filename size content-type)
                    (error (e)
                      (buffer-add-message buf
                        (make-message :text (format nil "Upload request error: ~a" e)
                                      :level :error))))))))))
      ((string= cmd "/0x0")
       (if (not args)
           (let ((buf (app-current-buffer app)))
             (when buf
               (buffer-add-message buf
                 (make-message :text "Usage: /0x0 <filepath>" :level :system))))
           (let ((file-path (string-trim " " args))
                 (buf (app-current-buffer app)))
             (if (not (probe-file file-path))
                 (when buf
                   (buffer-add-message buf
                     (make-message :text (format nil "File not found: ~a" file-path) :level :error)))
                 (when buf
                   (buffer-add-message buf
                     (make-message :text (format nil "Uploading to 0x0.st...") :level :system))
                   (let ((buf-ref buf)
                         (path (namestring (truename file-path))))
                     (bt:make-thread
                      (lambda ()
                        (handler-case
                            (let ((url (0x0-upload-file path)))
                              (if url
                                  (bt:with-lock-held ((app-event-lock app))
                                    (push (list :0x0-complete :url url :buffer buf-ref)
                                          (app-events app)))
                                  (bt:with-lock-held ((app-event-lock app))
                                    (push (list :0x0-error :message "No URL returned" :buffer buf-ref)
                                          (app-events app)))))
                          (error (e)
                            (bt:with-lock-held ((app-event-lock app))
                              (push (list :0x0-error :message (format nil "~a" e) :buffer buf-ref)
                                    (app-events app))))))
                      :name "0x0-upload")))))))
      ((string= cmd "/paste")
       (let ((buf (app-current-buffer app)))
         (when buf
           (buffer-add-message buf
             (make-message :text "Pasting clipboard to 0x0.st..." :level :system))
           (let ((buf-ref buf))
             (bt:make-thread
              (lambda ()
                (handler-case
                    (let* ((text (or
                                   ;; Try wl-paste first (Wayland)
                                   (ignore-errors
                                     (with-output-to-string (s)
                                       (let ((proc (sb-ext:run-program
                                                    "wl-paste" '("--no-newline")
                                                    :output s :error nil :search t)))
                                         (unless (zerop (sb-ext:process-exit-code proc))
                                           (error "wl-paste failed")))))
                                   ;; Fall back to xclip (X11)
                                   (ignore-errors
                                     (with-output-to-string (s)
                                       (let ((proc (sb-ext:run-program
                                                    "xclip" '("-selection" "clipboard" "-o")
                                                    :output s :error nil :search t)))
                                         (unless (zerop (sb-ext:process-exit-code proc))
                                           (error "xclip failed")))))
                                   ;; Fall back to xsel
                                   (ignore-errors
                                     (with-output-to-string (s)
                                       (let ((proc (sb-ext:run-program
                                                    "xsel" '("--clipboard" "--output")
                                                    :output s :error nil :search t)))
                                         (unless (zerop (sb-ext:process-exit-code proc))
                                           (error "xsel failed")))))
                                   (error "No clipboard tool available (install wl-paste, xclip, or xsel)")))
                           (text (string-trim '(#\Space #\Newline) text)))
                      (if (zerop (length text))
                          (bt:with-lock-held ((app-event-lock app))
                            (push (list :0x0-error :message "Clipboard is empty" :buffer buf-ref)
                                  (app-events app)))
                          (let ((url (0x0-upload-text text "paste.txt")))
                            (if url
                                (bt:with-lock-held ((app-event-lock app))
                                  (push (list :0x0-complete :url url :buffer buf-ref)
                                        (app-events app)))
                                (bt:with-lock-held ((app-event-lock app))
                                  (push (list :0x0-error :message "No URL returned" :buffer buf-ref)
                                        (app-events app)))))))
                  (error (e)
                    (bt:with-lock-held ((app-event-lock app))
                      (push (list :0x0-error :message (format nil "~a" e) :buffer buf-ref)
                            (app-events app))))))
              :name "0x0-paste")))))
      ((string= cmd "/help")
       (let ((buf (app-current-buffer app)))
         (when buf
           (buffer-add-message buf
             (make-message :text "Commands: /join /part /add /msg /omemo /upload /0x0 /paste /quit /help"
                           :level :system))))))))

(defun app-send-message (app text)
  "Send a message to the current buffer's XMPP connection."
  (let ((text (emoji-expand text))
        (buf (app-current-buffer app)))
    (when (and buf (not (eq (buffer-type buf) :system)))
      (let* ((jid (buffer-name buf))
             (conn (app-first-connection app))
             (use-omemo (and (buffer-omemo-p buf)
                             (eq (buffer-type buf) :dm)
                             *omemo-device-id*)))
        (debug-log "Send: to=~a type=~a omemo=~a text=~a" jid (buffer-type buf) use-omemo text)
        (when conn
          (let ((msg-id nil)
                (correcting (buffer-correcting-p buf))
                (replace-id (buffer-last-sent-id buf)))
            (handler-case
                (cond
                  ;; Correction of previous message
                  ((and correcting replace-id)
                   (setf msg-id (xmpp-send-correction conn jid text replace-id
                                   :type (if (eq (buffer-type buf) :muc) "groupchat" "chat"))))
                  ((eq (buffer-type buf) :muc)
                   (setf msg-id (xmpp-send-groupchat conn jid text)))
                  ;; OMEMO-encrypted DM
                  (use-omemo
                   (let* ((our-jid (bare-jid (conn-bound-jid conn)))
                          (encrypted-el (app-build-omemo-encrypted jid text our-jid)))
                     (if encrypted-el
                         (xmpp-send-omemo-message conn jid encrypted-el)
                         (progn
                           (debug-log "OMEMO encrypt failed, sending plaintext")
                           (setf msg-id (xmpp-send-message conn jid text))))))
                  ;; Plain DM
                  ((eq (buffer-type buf) :dm)
                   (setf msg-id (xmpp-send-message conn jid text))))
              (error (e)
                (debug-log "Send error: ~a" e)
                (buffer-add-message buf
                  (make-message :text (format nil "Send error: ~A" e) :level :error))))
            ;; Track last sent message for corrections
            (when msg-id
              (setf (buffer-last-sent-id buf) msg-id
                    (buffer-last-sent-text buf) text))
            ;; Clear correction state
            (setf (buffer-correcting-p buf) nil)))
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
                     (make-message :text "Disconnected from server" :level :error)))))
              (:upload-complete
               (let ((url (getf (rest evt) :url))
                     (buf (getf (rest evt) :buffer))
                     (conn (app-first-connection app)))
                 (when (and url buf conn)
                   ;; Send the download URL as a message with OOB element
                   (let* ((jid (buffer-name buf))
                          (msg-type (if (eq (buffer-type buf) :muc) "groupchat" "chat"))
                          (oob (make-xml-element "x"
                                 :namespace clabber.xmpp::+ns-oob+
                                 :children (list (make-xml-element "url" :text url))))
                          (body-el (make-xml-element "body" :text url))
                          (msg-el (make-xml-element "message"
                                    :attributes `(("to" . ,jid)
                                                  ("type" . ,msg-type)
                                                  ("id" . ,(generate-id)))
                                    :children (list body-el oob))))
                     (handler-case
                         (xmpp-stream-send (conn-stream conn) msg-el)
                       (error (e)
                         (debug-log "Send upload URL error: ~a" e))))
                   ;; Show locally for DMs
                   (when (eq (buffer-type buf) :dm)
                     (let ((nick (if conn
                                     (let ((bound (conn-bound-jid conn)))
                                       (if bound (parse-jid bound) "me"))
                                     "me")))
                       (buffer-add-message buf
                         (make-message :text url :nick nick))))
                   (buffer-add-message buf
                     (make-message :text "File uploaded successfully" :level :system)))))
              (:upload-error
               (let ((message (getf (rest evt) :message))
                     (buf (getf (rest evt) :buffer)))
                 (when buf
                   (buffer-add-message buf
                     (make-message :text (format nil "Upload failed: ~a" message)
                                   :level :error)))))
              (:0x0-complete
               (let ((url (getf (rest evt) :url))
                     (buf (getf (rest evt) :buffer)))
                 (when (and url buf)
                   (buffer-add-message buf
                     (make-message :text (format nil "0x0.st: ~a" url) :level :system))
                   ;; Send URL as message in current chat
                   (let ((conn (app-first-connection app)))
                     (when (and conn (not (eq (buffer-type buf) :system)))
                       (app-send-message app url))))))
              (:0x0-error
               (let ((message (getf (rest evt) :message))
                     (buf (getf (rest evt) :buffer)))
                 (when buf
                   (buffer-add-message buf
                     (make-message :text (format nil "0x0.st error: ~a" message)
                                   :level :error)))))))
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
       ;; Handle chat state notifications (XEP-0085)
       (let ((chat-state (clabber.xmpp::message-chat-state stanza)))
         (when (and from chat-state)
           (let ((bare (bare-jid from)))
             (cond
               ((string= chat-state "composing")
                (setf (gethash bare (app-typing-indicators app))
                      (cons (or (jid-resource from) bare) (get-universal-time))))
               (t
                (remhash bare (app-typing-indicators app)))))))
       ;; Handle MUC subject/topic changes
       (when (and from msg-type (string= msg-type "groupchat"))
         (let ((subj (clabber.xmpp::message-subject stanza)))
           (when subj
             (let* ((buf-name (bare-jid from))
                    (buf (app-find-buffer app buf-name)))
               (when (and buf (eq (buffer-type buf) :muc))
                 (setf (buffer-topic buf) subj)
                 (debug-log "MUC topic for ~a: ~a" buf-name subj))))))
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
                    (conn (app-first-connection app))
                    (our-jid (when conn (bare-jid (conn-bound-jid conn))))
                    (from-bare (bare-jid from))
                    (to-jid (stanza-to stanza))
                    (is-own-msg (and our-jid (string= from-bare our-jid) (not is-muc)))
                    (buf-name (if is-own-msg
                                  (when to-jid (bare-jid to-jid))
                                  from-bare))
                    (nick (cond (is-muc (jid-resource from))
                                (is-own-msg (or our-jid "me"))
                                (t (let ((pct (position #\% from-bare)))
                                     (if pct (subseq from-bare 0 pct) from-bare)))))
                    (buf (when buf-name
                           (app-find-or-create-buffer app buf-name
                             :type (if is-muc :muc :dm)
                             :display-name (when is-muc (strip-muc-name buf-name))))))
               (when buf
                 ;; Mark buffer as OMEMO-capable if we decrypted successfully
                 (when is-encrypted
                   (setf (buffer-omemo-p buf) t))
                 ;; Handle message correction (XEP-0308)
                 (let ((replace-id (clabber.xmpp::message-replace-id stanza))
                       (msg-ts (if delay
                                   (or (ignore-errors (parse-xmpp-timestamp delay))
                                       (get-universal-time))
                                   (get-universal-time))))
                   (if (and replace-id (buffer-correct-message buf replace-id display-body))
                       ;; Correction applied to existing message
                       (debug-log "Corrected message ~a" replace-id)
                       ;; New message
                       (buffer-add-message buf
                         (make-message :text display-body :nick nick
                                       :timestamp msg-ts
                                       :stanza-id (stanza-id stanza))))))))
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
                ;; Known MUC buffer: update participants with role
                ((and buf (eq (buffer-type buf) :muc))
                 (if (and type- (string= type- "unavailable"))
                     (buffer-remove-participant buf resource)
                     ;; Parse affiliation/role from <x xmlns="...muc#user"><item>
                     (let ((affiliation nil)
                           (role nil))
                       (when (stanza-xml stanza)
                         (let ((x-el (find-if (lambda (c)
                                                (and (typep c 'xml-element)
                                                     (string= (xml-name c) "x")
                                                     (search "muc#user" (or (xml-namespace c) ""))))
                                              (xml-children (stanza-xml stanza)))))
                           (when x-el
                             (let ((item (xml-child x-el "item")))
                               (when item
                                 (setf affiliation (xml-attr item "affiliation"))
                                 (setf role (xml-attr item "role")))))))
                       (buffer-add-participant buf resource
                         :role (affiliation-to-role affiliation role)))))
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
                     ;; Classify: #channel%server@gateway = MUC, everything else = DM
                     (let* ((at-pos (position #\@ jid))
                            (local-part (if at-pos (subseq jid 0 at-pos) jid))
                            (is-muc (or (and (search "#" local-part)
                                             (search "%" local-part))
                                        (search "conference" jid))))
                       (if is-muc
                           (handler-case
                               (when conn
                                 (debug-log "Roster: joining MUC ~a" jid)
                                 (xmpp-join-muc conn jid)
                                 (app-find-or-create-buffer app jid
                                   :type :muc
                                   :display-name (or name (strip-muc-name jid)))
                                 (xmpp-query-mam conn jid :max 50)
                                 (xmpp-disco-info conn jid))
                             (error (e)
                               (debug-log "Roster MUC join error for ~a: ~a" jid e)))
                           (app-find-or-create-buffer app jid
                             :type :dm :display-name (or name jid))))))))
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
                           (xmpp-query-mam conn jid :max 50)
                           (xmpp-disco-info conn jid))
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
                            ;; If this is OUR device list, merge our ID and republish
                            (when (and conn *omemo-device-id*
                                       (string= from-jid (bare-jid (conn-bound-jid conn)))
                                       (not (member *omemo-device-id* device-ids)))
                              (debug-log "OMEMO: our device ~a not in list, republishing merged" *omemo-device-id*)
                              (handler-case
                                  (xmpp-publish-omemo-devicelist conn *omemo-device-id* device-ids)
                                (error (e) (debug-log "OMEMO: republish error: ~a" e))))
                            ;; Auto-fetch bundles for each device
                            (when conn
                              (dolist (did device-ids)
                                (unless (or (eql did *omemo-device-id*)
                                            (signal-has-session-p from-jid did))
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
         ;; Handle IQ errors for device list fetch (node may not exist yet)
         (when (and (string= type- "error") conn *omemo-device-id*)
           (let ((id (stanza-id stanza)))
             (when (and id (search "pep-fetch" id))
               (debug-log "OMEMO: device list fetch error (node may not exist), publishing just our ID")
               (handler-case
                   (xmpp-publish-omemo-devicelist conn *omemo-device-id* nil)
                 (error (e) (debug-log "OMEMO: fallback publish error: ~a" e))))))
         ;; Handle disco#info response - extract MUC features/modes + HTTP Upload
         (when (and (string= type- "result")
                    (string= (xml-name query) "query")
                    (string= (or (xml-namespace query) "") clabber.xmpp::+ns-disco-info+))
           (let ((from-jid (stanza-from stanza)))
             (when from-jid
               ;; MUC mode flags
               (let ((buf (app-find-buffer app (bare-jid from-jid))))
                 (when (and buf (eq (buffer-type buf) :muc))
                   (let ((features nil))
                     (dolist (child (xml-children query))
                       (when (and (typep child 'xml-element)
                                  (string= (xml-name child) "feature"))
                         (let ((var (xml-attr child "var")))
                           (when var (push var features)))))
                     (let ((modes ""))
                       (when (member "muc_noexternal" features :test #'string=)
                         (setf modes (concatenate 'string modes "n")))
                       (when (member "muc_membersonly" features :test #'string=)
                         (setf modes (concatenate 'string modes "i")))
                       (when (member "muc_moderated" features :test #'string=)
                         (setf modes (concatenate 'string modes "m")))
                       (when (member "muc_nosubject" features :test #'string=)
                         (setf modes (concatenate 'string modes "t")))
                       (when (member "muc_hidden" features :test #'string=)
                         (setf modes (concatenate 'string modes "s")))
                       (when (> (length modes) 0)
                         (setf (buffer-modes buf) (concatenate 'string "+" modes))
                         (debug-log "Disco: ~a modes=+~a" (bare-jid from-jid) modes))))))
               ;; Check if this component supports HTTP Upload
               (dolist (child (xml-children query))
                 (when (and (typep child 'xml-element)
                            (string= (xml-name child) "feature")
                            (string= (or (xml-attr child "var") "")
                                     clabber.xmpp::+ns-http-upload+))
                   (setf *upload-service-jid* from-jid)
                   (debug-log "Discovered HTTP Upload service: ~a" from-jid))))))
         ;; Handle disco#items response - discover server components
         (when (and (string= type- "result")
                    (string= (xml-name query) "query")
                    (string= (or (xml-namespace query) "") clabber.xmpp::+ns-disco-items+))
           (dolist (child (xml-children query))
             (when (and (typep child 'xml-element)
                        (string= (xml-name child) "item"))
               (let ((item-jid (xml-attr child "jid")))
                 (when item-jid
                   (debug-log "Disco item: ~a" item-jid)
                   (handler-case
                       (xmpp-disco-info-for-upload conn item-jid)
                     (error (e) (debug-log "Disco info error for ~a: ~a" item-jid e))))))))
         ;; Handle HTTP Upload slot response (XEP-0363)
         (when (and (string= type- "result")
                    (string= (xml-name query) "slot"))
           (handle-upload-slot-response app stanza))
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
        ;; Query MAM for DM history on new buffer creation
        (when (eq type :dm)
          (let ((conn (app-first-connection app)))
            (when conn
              (handler-case
                  (xmpp-query-mam conn nil :with name :max 50)
                (error (e) (debug-log "DM MAM query error for ~a: ~a" name e))))))
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

(defun app-build-omemo-encrypted (to-jid plaintext &optional our-jid)
  "Build an OMEMO <encrypted> xml-element for sending to TO-JID.
   Also encrypts for our own other devices (OUR-JID) so they can read sent messages.
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
            ;; Also encrypt for our own other devices
            (when our-jid
              (let ((own-devices (get-cached-device-list our-jid)))
                (dolist (device-id own-devices)
                  (unless (= device-id *omemo-device-id*)
                    (handler-case
                        (multiple-value-bind (encrypted-key msg-type)
                            (signal-session-encrypt our-jid device-id key-material)
                          (push (make-xml-element "key"
                                  :attributes `(("rid" . ,(princ-to-string device-id))
                                                ,@(when (= msg-type +ciphertext-prekey-type+)
                                                    '(("prekey" . "true"))))
                                  :text (bytes-to-base64 encrypted-key))
                                key-elements))
                      (error (e)
                        (debug-log "OMEMO: can't encrypt to own ~a/~a: ~a" our-jid device-id e)))))))
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
              (handler-case (xmpp-enable-carbons conn) (error () nil))
              ;; Discover server components for HTTP Upload (XEP-0363)
              (let* ((jid (conn-bound-jid conn))
                     (at (when jid (position #\@ jid)))
                     (server (when at (subseq jid (1+ at))))
                     (slash (when server (position #\/ server)))
                     (domain (if slash (subseq server 0 slash) server)))
                (when domain
                  (handler-case
                      (xmpp-disco-items conn domain)
                    (error (e) (debug-log "Disco items error: ~a" e))))))
          (error () nil))

        ;; Step 4: Joining rooms (will happen async via bookmarks)
        (app-splash-step app "Joining rooms")

        ;; Step 5: Initialize OMEMO
        (app-splash-step app "Initializing OMEMO")
        (handler-case
            (progn
              (omemo-init)
              (debug-log "OMEMO initialized, device-id=~a" *omemo-device-id*)
              (when *omemo-device-id*
                ;; Publish bundle (this is safe - doesn't overwrite anything)
                (let ((bundle-xml (omemo-bundle-xml-element)))
                  (when bundle-xml
                    (xmpp-publish-omemo-bundle conn *omemo-device-id* bundle-xml)))
                ;; Fetch our own device list - the async PubSub handler will:
                ;; 1. Cache the existing device IDs
                ;; 2. Merge our ID and republish the device list
                (let ((our-jid (bare-jid (conn-bound-jid conn))))
                  (when our-jid
                    (xmpp-fetch-omemo-devicelist conn our-jid)
                    (debug-log "OMEMO: requested own device list for ~a (will merge async)" our-jid)))
                (debug-log "OMEMO bundle published, device list merge pending")))
          (error (e)
            (debug-log "OMEMO init error (non-fatal): ~a" e)))

        ;; Final splash step - mark OMEMO done
        (app-splash-step app "Ready")

        ;; Mark running BEFORE starting receive thread
        (setf (app-running-p app) t)

        ;; Start receive thread
        (app-start-receive-thread app conn))

      (sleep 0.3)
      ;; Clear screen immediately after sleep to prevent flash
      (clear-screen)
      (force-output *terminal-io*))

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
           ;; XMPP keepalive ping (XEP-0199) every 60 seconds
           (let ((now (get-universal-time)))
             (when (> (- now (app-last-ping-time app)) 60)
               (let ((conn (app-first-connection app)))
                 (when (and conn (conn-connected-p conn))
                   (handler-case
                       (progn
                         (xmpp-send-ping conn)
                         (setf (app-last-ping-time app) now))
                     (error (e)
                       (debug-log "Ping error: ~a" e)))))))
           ;; Re-render (catch errors to avoid spewing to terminal)
           (handler-case (app-render app)
             (error (e)
               (debug-log "Render error: ~a" e))))
      ;; Cleanup
      (setf (app-running-p app) nil)
      (let ((conn (app-first-connection app)))
        (when conn
          (handler-case (xmpp-disconnect conn) (error () nil)))))))
