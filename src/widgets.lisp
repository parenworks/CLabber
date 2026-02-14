(in-package #:clabber.widgets)

;;; Widget System for CLabber UI
;;; Panels, input bar, status bar, buffer bar, splash screen

;;; Base panel class

(defclass panel ()
  ((x :initarg :x :accessor panel-x :initform 1)
   (y :initarg :y :accessor panel-y :initform 1)
   (width :initarg :width :accessor panel-width :initform 40)
   (height :initarg :height :accessor panel-height :initform 10)
   (title :initarg :title :accessor panel-title :initform nil)
   (active-p :initarg :active :accessor panel-active-p :initform nil)
   (dirty-p :initform t :accessor panel-dirty-p
            :documentation "Whether panel needs redraw"))
  (:documentation "Base panel widget"))

(defgeneric panel-render (panel)
  (:documentation "Render the panel to the terminal"))

(defun draw-box (x y w h &key title active-p)
  "Draw a bordered box at position (x, y) with dimensions w x h."
  (let* ((theme (current-theme))
         (border-color (if active-p
                           (theme-border-active theme)
                           (theme-border-inactive theme))))
    ;; Top border
    (cursor-to y x)
    (when border-color (emit-fg border-color *terminal-io*))
    (princ (theme-box-tl theme) *terminal-io*)
    (when title
      (let* ((max-title-w (- w 4))
             (display-title (if (> (length title) max-title-w)
                                (subseq title 0 max-title-w)
                                title)))
        (princ (theme-box-h theme) *terminal-io*)
        (when active-p (bold))
        (princ display-title *terminal-io*)
        (when active-p (reset) (when border-color (emit-fg border-color *terminal-io*)))
        (loop repeat (- w 2 1 (length display-title))
              do (princ (theme-box-h theme) *terminal-io*))))
    (unless title
      (loop repeat (- w 2) do (princ (theme-box-h theme) *terminal-io*)))
    (princ (theme-box-tr theme) *terminal-io*)
    ;; Side borders
    (loop for row from (1+ y) below (+ y h -1) do
      (cursor-to row x)
      (princ (theme-box-v theme) *terminal-io*)
      (cursor-to row (+ x w -1))
      (princ (theme-box-v theme) *terminal-io*))
    ;; Bottom border
    (cursor-to (+ y h -1) x)
    (princ (theme-box-bl theme) *terminal-io*)
    (loop repeat (- w 2) do (princ (theme-box-h theme) *terminal-io*))
    (princ (theme-box-br theme) *terminal-io*)
    (reset)))

(defun clear-panel-content (x y w h)
  "Clear the interior of a panel."
  (loop for row from (1+ y) below (+ y h -1) do
    (cursor-to row (1+ x))
    (princ (make-string (- w 2) :initial-element #\Space) *terminal-io*)))

;;; Chat panel - displays messages

(defclass chat-panel (panel)
  ((buffer :initarg :buffer :accessor chat-panel-buffer :initform nil))
  (:documentation "Panel displaying chat messages"))

(defmethod panel-render ((panel chat-panel))
  (let ((x (panel-x panel))
        (y (panel-y panel))
        (w (panel-width panel))
        (h (panel-height panel))
        (buf (chat-panel-buffer panel))
        (theme (current-theme)))
    ;; Draw border with title (include participant count + topic for MUCs)
    (draw-box x y w h
              :title (when buf
                       (let* ((name (or (clabber.model:buffer-display-name buf)
                                        (clabber.model:buffer-name buf)))
                              (modes (clabber.model:buffer-modes buf))
                              (topic (clabber.model:buffer-topic buf))
                              (nparts (length (clabber.model:buffer-participants buf)))
                              (header (cond
                                        ;; Modes available (non-biboumi MUCs)
                                        (modes (format nil "~a [~a]" name modes))
                                        ;; Show participant count for MUCs
                                        ((and (eq (clabber.model:buffer-type buf) :muc)
                                              (> nparts 0))
                                         (format nil "~a (~d)" name nparts))
                                        (t name))))
                         (if (and topic (> (length topic) 0))
                             (let* ((max-topic (- w (length header) 5))
                                    (short-topic (if (> (length topic) max-topic)
                                                     (concatenate 'string
                                                       (subseq topic 0 (max 0 (- max-topic 3))) "...")
                                                     topic)))
                               (format nil "~a | ~a" header short-topic))
                             header)))
              :active-p (panel-active-p panel))
    ;; Clear content area
    (clear-panel-content x y w h)
    ;; Render messages (newest at top)
    (when (and buf (> (fill-pointer (clabber.model:buffer-messages buf)) 0))
      (let* ((msgs (clabber.model:buffer-messages buf))
             (count (fill-pointer msgs))
             (content-h (- h 2))
             (content-w (- w 2))
             (offset (clabber.model:buffer-scroll-offset buf))
             ;; Start from newest, walk backwards
             (start-idx (min (1- count) (max 0 (- count 1 offset))))
             (row (1+ y)))
        (loop for i from start-idx downto 0
              for msg = (aref msgs i)
              while (< row (+ y h -1))
              do (let* ((ts (clabber.model:message-timestamp msg))
                        (nick (clabber.model:message-nick msg))
                        (text (clabber.model:message-text msg))
                        (level (clabber.model:message-level msg))
                        (lvl-color (theme-level-color theme level))
                        (prefix-len (+ 8 (if nick (+ (length nick) 2) 0)))
                        (text-width (max 1 (- content-w prefix-len)))
                        ;; Calculate how many rows this message needs
                        (text-len (length text))
                        (num-lines (max 1 (ceiling text-len text-width))))
                   ;; First line: timestamp + nick + start of text
                   (cursor-to row (+ x 1))
                   ;; Timestamp
                   (emit-fg (theme-timestamp theme) *terminal-io*)
                   (multiple-value-bind (sec min hour)
                       (decode-universal-time ts)
                     (declare (ignore sec))
                     (format *terminal-io* "[~2,'0D:~2,'0D] " hour min))
                   (reset)
                   ;; Nick
                   (when nick
                     (let ((nick-color (or lvl-color
                                           (theme-nick-color theme nick))))
                       (when nick-color (emit-fg nick-color *terminal-io*))
                       (when (and (not lvl-color) (not (clabber.model:message-highlight-p msg)))
                         (bold))
                       (format *terminal-io* "~A" nick)
                       (reset)
                       (princ ": " *terminal-io*)))
                   ;; Message text with wrapping
                   (when lvl-color (emit-fg lvl-color *terminal-io*))
                   (when (clabber.model:message-highlight-p msg)
                     (emit-fg (theme-mention-indicator theme) *terminal-io*)
                     (bold))
                   ;; First line of text
                   (let ((first-chunk (subseq text 0 (min text-len text-width))))
                     (princ first-chunk *terminal-io*))
                   (reset)
                   (incf row)
                   ;; Continuation lines (indented under first letter of text)
                   (loop for line-idx from 1 below num-lines
                         while (< row (+ y h -1))
                         for start = (* line-idx text-width)
                         for end = (min text-len (* (1+ line-idx) text-width))
                         do (cursor-to row (+ x 1))
                            ;; Indent with spaces to align under message text
                            (dotimes (j prefix-len) (princ #\Space *terminal-io*))
                            (when lvl-color (emit-fg lvl-color *terminal-io*))
                            (when (clabber.model:message-highlight-p msg)
                              (emit-fg (theme-mention-indicator theme) *terminal-io*)
                              (bold))
                            (princ (subseq text start end) *terminal-io*)
                            (reset)
                            (incf row))))
        ;; OMEMO indicator in top-right corner
        (when (clabber.model:buffer-omemo-p buf)
          (cursor-to y (- (+ x w) 4))
          (emit-fg (theme-omemo-lock theme) *terminal-io*)
          (princ "🔒" *terminal-io*)
          (reset))))))

;;; Roster panel - DMs and MUCs sidebar

(defclass roster-panel (panel)
  ((dm-buffers :initarg :dm-buffers :accessor roster-dm-buffers :initform nil)
   (muc-buffers :initarg :muc-buffers :accessor roster-muc-buffers :initform nil)
   (selected-index :initarg :selected :accessor roster-selected-index :initform 0))
  (:documentation "Left sidebar showing DMs and MUCs"))

(defmethod panel-render ((panel roster-panel))
  (let ((x (panel-x panel))
        (y (panel-y panel))
        (w (panel-width panel))
        (h (panel-height panel))
        (theme (current-theme)))
    (draw-box x y w h :title "ROOMS" :active-p (panel-active-p panel))
    (clear-panel-content x y w h)
    (let ((row (1+ y))
          (content-w (- w 2))
          (idx 0))
      ;; DMs section
      (cursor-to row (+ x 1))
      (emit-fg (theme-border-active theme) *terminal-io*)
      (bold)
      (princ "DMs" *terminal-io*)
      (reset)
      (incf row)
      (dolist (buf (roster-dm-buffers panel))
        (when (< row (+ y h -1))
          (cursor-to row (+ x 1))
          (let ((selected-p (= idx (roster-selected-index panel)))
                (has-unread (> (clabber.model:buffer-unread-count buf) 0))
                (has-mention (clabber.model:buffer-mention-p buf))
                (name (or (clabber.model:buffer-display-name buf)
                          (clabber.model:buffer-name buf))))
            (when selected-p (inverse))
            ;; Unread indicator
            (cond
              (has-mention
               (emit-fg (theme-mention-indicator theme) *terminal-io*)
               (princ "● " *terminal-io*))
              (has-unread
               (emit-fg (theme-unread-indicator theme) *terminal-io*)
               (princ "● " *terminal-io*))
              (t (princ "  " *terminal-io*)))
            ;; Name (truncate)
            (let ((max-name (- content-w 2)))
              (princ (if (> (length name) max-name)
                         (subseq name 0 max-name)
                         name)
                     *terminal-io*))
            (reset))
          (incf row)
          (incf idx)))
      ;; Blank line separator
      (when (< row (+ y h -2))
        (incf row))
      ;; MUCs section
      (when (< row (+ y h -1))
        (cursor-to row (+ x 1))
        (emit-fg (theme-border-active theme) *terminal-io*)
        (bold)
        (princ "MUCs" *terminal-io*)
        (reset)
        (incf row))
      (dolist (buf (roster-muc-buffers panel))
        (when (< row (+ y h -1))
          (cursor-to row (+ x 1))
          (let ((selected-p (= idx (roster-selected-index panel)))
                (has-unread (> (clabber.model:buffer-unread-count buf) 0))
                (has-mention (clabber.model:buffer-mention-p buf))
                (name (or (clabber.model:buffer-display-name buf)
                          (clabber.model:buffer-name buf))))
            (when selected-p (inverse))
            (cond
              (has-mention
               (emit-fg (theme-mention-indicator theme) *terminal-io*)
               (princ "● " *terminal-io*))
              (has-unread
               (emit-fg (theme-unread-indicator theme) *terminal-io*)
               (princ "● " *terminal-io*))
              (t (princ "  " *terminal-io*)))
            (let ((max-name (- content-w 2)))
              (princ (if (> (length name) max-name)
                         (subseq name 0 max-name)
                         name)
                     *terminal-io*))
            (reset))
          (incf row)
          (incf idx))))))

;;; Participants panel - right sidebar for MUC members

(defun role-sort-key (role)
  "Return numeric sort key for MUC roles (higher = more important)."
  (case role
    (:owner 4)
    (:admin 3)
    (:moderator 2)
    (:participant 1)
    (t 0)))

(defclass participants-panel (panel)
  ((participants :initarg :participants :accessor participants-list :initform nil))
  (:documentation "Right sidebar showing MUC participants"))

(defmethod panel-render ((panel participants-panel))
  (let ((x (panel-x panel))
        (y (panel-y panel))
        (w (panel-width panel))
        (h (panel-height panel))
        (theme (current-theme)))
    (draw-box x y w h :title "WHO" :active-p nil)
    (clear-panel-content x y w h)
    (let ((row (1+ y))
          (content-w (- w 2))
          (entries (participants-list panel)))
      ;; Sort by role priority: owner > admin > moderator > participant
      (let ((sorted (sort (copy-list entries)
                          (lambda (a b)
                            (let ((ra (clabber.model:participant-role a))
                                  (rb (clabber.model:participant-role b)))
                              (or (> (role-sort-key ra) (role-sort-key rb))
                                  (and (= (role-sort-key ra) (role-sort-key rb))
                                       (string< (clabber.model:participant-nick a)
                                                (clabber.model:participant-nick b)))))))))
        (dolist (entry sorted)
          (when (< row (+ y h -1))
            (cursor-to row (+ x 1))
            (let* ((nick (clabber.model:participant-nick entry))
                   (role (clabber.model:participant-role entry))
                   (prefix (clabber.model:role-prefix role))
                   (nick-color (theme-nick-color theme nick)))
              ;; Role prefix in a distinct color
              (when (> (length prefix) 0)
                (emit-fg (theme-system-color theme) *terminal-io*)
                (bold)
                (princ prefix *terminal-io*)
                (reset))
              ;; Nick
              (when nick-color (emit-fg nick-color *terminal-io*))
              (let ((max-nick (- content-w (length prefix))))
                (princ (if (> (length nick) max-nick)
                           (subseq nick 0 max-nick)
                           nick)
                       *terminal-io*))
              (reset))
            (incf row)))))))

;;; Input bar - text input at top of screen

(defclass input-bar (panel)
  ((text :initform "" :accessor input-bar-text)
   (cursor-pos :initform 0 :accessor input-bar-cursor-pos)
   (history :initform nil :accessor input-bar-history)
   (history-index :initform -1 :accessor input-bar-history-index))
  (:documentation "Text input bar at top of screen"))

(defmethod panel-render ((panel input-bar))
  (let ((x (panel-x panel))
        (y (panel-y panel))
        (w (panel-width panel))
        (theme (current-theme)))
    (cursor-to y x)
    (clear-line)
    ;; Prompt
    (emit-fg (theme-border-active theme) *terminal-io*)
    (bold)
    (princ (theme-input-prompt theme) *terminal-io*)
    (reset)
    ;; Input text
    (let* ((prompt-len (length (theme-input-prompt theme)))
           (max-text (- w prompt-len 1))
           (text (input-bar-text panel))
           (display-text (if (> (length text) max-text)
                             (subseq text (max 0 (- (length text) max-text)))
                             text)))
      (princ display-text *terminal-io*)
      (clear-to-end))
    ;; Position cursor
    (let* ((prompt-len (length (theme-input-prompt theme)))
           (cursor-col (+ x prompt-len (min (input-bar-cursor-pos panel)
                                             (- w prompt-len 1)))))
      (cursor-to y cursor-col)
      (cursor-show))))

;;; Status bar - shows current context info

(defclass status-bar (panel)
  ((left-text :initform "" :accessor status-bar-left)
   (center-text :initform "" :accessor status-bar-center)
   (right-text :initform "" :accessor status-bar-right))
  (:documentation "Status bar below input"))

(defmethod panel-render ((panel status-bar))
  (let ((x (panel-x panel))
        (y (panel-y panel))
        (w (panel-width panel))
        (theme (current-theme)))
    (cursor-to y x)
    ;; Full-width background
    (emit-bg (theme-border-inactive theme) *terminal-io*)
    (emit-fg (theme-fg theme) *terminal-io*)
    (princ (make-string w :initial-element #\Space) *terminal-io*)
    ;; Left text
    (cursor-to y x)
    (bold)
    (princ (status-bar-left panel) *terminal-io*)
    (reset)
    ;; Center text
    (let ((center-x (+ x (floor (- w (length (status-bar-center panel))) 2))))
      (cursor-to y center-x)
      (emit-bg (theme-border-inactive theme) *terminal-io*)
      (emit-fg (theme-fg theme) *terminal-io*)
      (princ (status-bar-center panel) *terminal-io*))
    ;; Right text
    (let ((right-x (+ x (- w (length (status-bar-right panel))))))
      (cursor-to y right-x)
      (emit-bg (theme-border-inactive theme) *terminal-io*)
      (emit-fg (theme-fg theme) *terminal-io*)
      (princ (status-bar-right panel) *terminal-io*))
    (reset)))

;;; Buffer bar - tab-style buffer switcher at bottom

(defclass buffer-bar (panel)
  ((buffers :initarg :buffers :accessor buffer-bar-buffers :initform nil)
   (active-index :initarg :active :accessor buffer-bar-active-index :initform 0))
  (:documentation "Buffer bar at bottom of screen"))

(defmethod panel-render ((panel buffer-bar))
  (let ((x (panel-x panel))
        (y (panel-y panel))
        (w (panel-width panel))
        (theme (current-theme)))
    (cursor-to y x)
    ;; Full-width background
    (emit-bg (theme-border-inactive theme) *terminal-io*)
    (princ (make-string w :initial-element #\Space) *terminal-io*)
    (cursor-to y x)
    (let ((col x))
      (loop for buf in (buffer-bar-buffers panel)
            for idx from 0
            while (< col (+ x w -2))
            do (let* ((name (or (clabber.model:buffer-display-name buf)
                                (clabber.model:buffer-name buf)))
                      (active-p (= idx (buffer-bar-active-index panel)))
                      (has-unread (> (clabber.model:buffer-unread-count buf) 0))
                      (has-mention (clabber.model:buffer-mention-p buf))
                      (display-name (if (> (length name) 12)
                                        (subseq name 0 12)
                                        name)))
                 (cursor-to y col)
                 (emit-bg (theme-border-inactive theme) *terminal-io*)
                 ;; Unread indicator
                 (cond
                   (has-mention
                    (emit-fg (theme-mention-indicator theme) *terminal-io*)
                    (princ "●" *terminal-io*))
                   (has-unread
                    (emit-fg (theme-unread-indicator theme) *terminal-io*)
                    (princ "●" *terminal-io*))
                   (t (princ " " *terminal-io*)))
                 ;; Buffer name
                 (if active-p
                     (progn
                       (emit-fg (theme-border-active theme) *terminal-io*)
                       (bold))
                     (emit-fg (theme-fg theme) *terminal-io*))
                 (princ display-name *terminal-io*)
                 (reset)
                 (incf col (+ 1 (length display-name) 1)))))
    (reset)))

;;; Splash screen - shown during startup

(defclass splash-screen ()
  ((steps :initform '(("Connecting to server" . :pending)
                       ("Authenticating" . :pending)
                       ("Loading roster" . :pending)
                       ("Joining rooms" . :pending)
                       ("Initializing OMEMO" . :pending))
          :accessor splash-steps)
   (current-step :initform 0 :accessor splash-current-step))
  (:documentation "Startup splash screen with connection progress"))

(defun render-splash (splash width height)
  "Render the splash screen centered on the terminal."
  (let* ((theme (current-theme))
         (box-w 40)
         (box-h (+ 8 (length (splash-steps splash))))
         (box-x (max 1 (floor (- width box-w) 2)))
         (box-y (max 1 (floor (- height box-h) 2))))
    ;; Clear screen with theme background
    (emit-bg (theme-bg theme) *terminal-io*)
    (clear-screen)
    ;; Draw box
    (draw-box box-x box-y box-w box-h :active-p t)
    ;; Title
    (let ((title "C L a b b e r")
          (subtitle "XMPP Chat Client")
          (title-y (+ box-y 2)))
      (cursor-to title-y (+ box-x (floor (- box-w (length title)) 2)))
      (emit-fg (theme-border-active theme) *terminal-io*)
      (bold)
      (princ title *terminal-io*)
      (reset)
      (cursor-to (1+ title-y) (+ box-x (floor (- box-w (length subtitle)) 2)))
      (emit-fg (theme-timestamp theme) *terminal-io*)
      (princ subtitle *terminal-io*)
      (reset))
    ;; Steps
    (let ((step-y (+ box-y 5)))
      (loop for (label . status) in (splash-steps splash)
            for idx from 0
            do (cursor-to (+ step-y idx) (+ box-x 4))
               (case status
                 (:done
                  (emit-fg (theme-omemo-lock theme) *terminal-io*)
                  (princ "● " *terminal-io*))
                 (:active
                  (emit-fg (theme-unread-indicator theme) *terminal-io*)
                  (princ "◉ " *terminal-io*))
                 (:error
                  (emit-fg (theme-mention-indicator theme) *terminal-io*)
                  (princ "✗ " *terminal-io*))
                 (t
                  (emit-fg (theme-timestamp theme) *terminal-io*)
                  (princ "○ " *terminal-io*)))
               (princ label *terminal-io*)
               (reset)))
    (force-output *terminal-io*)))

(defun splash-advance (splash)
  "Mark current step as done and advance to next."
  (let ((steps (splash-steps splash))
        (idx (splash-current-step splash)))
    (when (< idx (length steps))
      (setf (cdr (nth idx steps)) :done)
      (incf (splash-current-step splash))
      (when (< (1+ idx) (length steps))
        (setf (cdr (nth (1+ idx) steps)) :active)))))

(defun splash-error (splash message)
  "Mark current step as error."
  (let ((steps (splash-steps splash))
        (idx (splash-current-step splash)))
    (when (< idx (length steps))
      (setf (cdr (nth idx steps)) :error)
      (setf (car (nth idx steps)) message))))
