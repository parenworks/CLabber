;;;; chat.lisp - Chat pane widget for CLabber

(in-package #:clabber)

(defclass chat-widget (widget)
  ((pane :initarg :pane :reader chat-pane)))

(defun parse-nick-from-line (line)
  "Extract nick from a chat line like 'nick: message' or 'me: message'."
  (let ((colon-pos (position #\: line)))
    (when colon-pos
      (subseq line 0 colon-pos))))

(defun nick-color (nick)
  "Get color for a nick using the current theme."
  (if (or (null nick) (string= nick "me"))
      :cyan  ; Own messages in cyan
      (theme-nick-color (current-theme) nick)))

(defmethod render ((w chat-widget) scr rect st ly ui)
  (declare (ignore ui))
  (let* ((pane (chat-pane w))
         (buf-id (pane-buffer-id ly pane))
         (buf (state-get-buffer st buf-id))
         (focused (eql (layout-focused-pane ly)
                       (if (eql pane :chat-a) :chat-a :chat-b)))
         (title (format nil "~a~a"
                        (if buf (buffer-title buf) (princ-to-string buf-id))
                        (if focused " *" ""))))
    (draw-box scr rect title)
    (when buf
      (let* ((lines (buffer-lines buf))  ; Already newest-first (pushed)
             (max-lines (max 0 (- (rh rect) 2)))
             (max-cols (max 0 (- (rw rect) 2)))
             (visible-lines (subseq lines 0 (min (length lines) max-lines))))
        ;; Render newest messages at top
        (loop for i from 0
              for line in visible-lines do
          (setf (de.anvi.croatoan:cursor-position scr) (list (+ (ry rect) 1 i) (+ (rx rect) 1)))
          ;; Strip newlines and truncate to fit
          (let* ((clean-line (substitute #\Space #\Newline line))
                 (nick (parse-nick-from-line clean-line))
                 (color (nick-color nick))
                 (display-line (subseq clean-line 0 (min (length clean-line) max-cols))))
            (de.anvi.croatoan:add-string scr display-line :fgcolor color)))))))

(defmethod handle-input ((w chat-widget) key st ly ui)
  (declare (ignore w key st ly ui))
  nil)
