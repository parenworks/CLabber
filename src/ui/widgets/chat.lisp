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

(defun get-typing-users-display (buf)
  "Get display string for users currently typing."
  (when buf
    (let ((typing (buffer-typing-users buf))
          (nicks nil))
      (maphash (lambda (jid timestamp)
                 (declare (ignore timestamp))
                 ;; Extract nick from JID (resource part or bare JID)
                 (let ((nick (or (jid-resource jid) (bare-jid jid))))
                   (push nick nicks)))
               typing)
      (when nicks
        (if (= (length nicks) 1)
            (format nil "~a is typing..." (first nicks))
            (format nil "~{~a~^, ~} are typing..." nicks))))))

(defmethod render ((w chat-widget) scr rect st ly ui)
  (declare (ignore ui))
  (let* ((pane (chat-pane w))
         (buf-id (pane-buffer-id ly pane))
         (buf (state-get-buffer st buf-id))
         (focused (eql (layout-focused-pane ly)
                       (if (eql pane :chat-a) :chat-a :chat-b)))
         (typing-str (get-typing-users-display buf))
         (title (format nil "~a~a"
                        (if buf (buffer-title buf) (princ-to-string buf-id))
                        (if focused " *" ""))))
    (draw-box scr rect title)
    (when buf
      (let* ((lines (buffer-lines buf))  ; Already newest-first (pushed)
             ;; Reserve one line for typing indicator if someone is typing
             (typing-line-reserved (if typing-str 1 0))
             (max-lines (max 0 (- (rh rect) 2 typing-line-reserved)))
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
            (de.anvi.croatoan:add-string scr display-line :fgcolor color)))
        ;; Show typing indicator at bottom of chat area
        (when typing-str
          (let ((typing-y (+ (ry rect) (- (rh rect) 2)))
                (typing-display (subseq typing-str 0 (min (length typing-str) max-cols))))
            (setf (de.anvi.croatoan:cursor-position scr) (list typing-y (+ (rx rect) 1)))
            (de.anvi.croatoan:add-string scr typing-display :fgcolor :yellow)))))))

(defmethod handle-input ((w chat-widget) key st ly ui)
  (declare (ignore w key st ly ui))
  nil)
