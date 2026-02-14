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
   (splash :initform nil :accessor app-splash))
  (:documentation "Main CLabber application state"))

(defun make-app ()
  (make-instance 'application))

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
      (setf (roster-dm-buffers (layout-roster ly)) (app-dm-buffers app))
      (setf (roster-muc-buffers (layout-roster ly)) (app-muc-buffers app))
      (setf (roster-selected-index (layout-roster ly)) (app-active-buffer app)))
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
      ((string= cmd "/help")
       (let ((buf (app-current-buffer app)))
         (when buf
           (buffer-add-message buf
             (make-message :text "Commands: /join <room> /msg <jid> /quit /help"
                           :level :system))))))))

(defun app-send-message (app text)
  "Send a message to the current buffer."
  (let ((buf (app-current-buffer app)))
    (when buf
      ;; Add to local buffer
      (buffer-add-message buf
        (make-message :text text :nick "me"))
      ;; TODO: Send via XMPP connection
      )))

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
;;; Main event loop
;;; ============================================================

(defun app-run (app)
  "Main application loop."
  (with-raw-terminal
    ;; Get terminal size
    (let ((size (terminal-size)))
      (setf (app-width app) (first size)
            (app-height app) (second size)))

    ;; Show splash
    (app-show-splash app)

    ;; Connection phase (simulated for now)
    (app-splash-step app "Connecting to server")
    (sleep 0.3)
    (app-splash-step app "Authenticating")
    (sleep 0.3)
    (app-splash-step app "Loading roster")
    (sleep 0.3)
    (app-splash-step app "Joining rooms")
    (sleep 0.3)
    (app-splash-step app "Initializing OMEMO")
    (sleep 0.5)

    ;; Create system buffer
    (app-add-buffer app (make-buffer :name "*system*" :display-name "*system*" :type :system))
    (buffer-add-message (app-current-buffer app)
      (make-message :text (format nil "Welcome to CLabber ~A" *version*)
                    :level :system))

    ;; Compute layout and render
    (app-update-layout app)
    (setf (app-running-p app) t)

    ;; Main loop
    (loop while (app-running-p app) do
      (let ((key (read-key-with-timeout 33))) ; ~30fps
        (when key
          (unless (app-handle-input app key)
            (setf (app-running-p app) nil)))
        ;; Re-render
        (app-render app)))))
