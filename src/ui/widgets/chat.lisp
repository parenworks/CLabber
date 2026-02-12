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

(defun find-message-indent (line)
  "Find the column where message text starts after '[timestamp] nick: '.
   Returns the indent width, or 0 if no pattern found."
  (let ((colon-space (search ": " line)))
    (if colon-space
        (min (+ colon-space 2) (length line))
        0)))

(defun wrap-text (text width)
  "Word-wrap TEXT to fit WIDTH columns. Returns list of strings."
  (when (<= width 0) (return-from wrap-text (list "")))
  (if (<= (length text) width)
      (list text)
      (let ((result nil)
            (start 0)
            (len (length text)))
        (loop while (< start len) do
          (let ((end (min (+ start width) len)))
            (if (>= end len)
                (progn (push (subseq text start end) result)
                       (setf start end))
                (let ((break-pos (position #\Space text :start start :end end :from-end t)))
                  (if (and break-pos (> break-pos start))
                      (progn (push (subseq text start break-pos) result)
                             (setf start (1+ break-pos)))
                      (progn (push (subseq text start end) result)
                             (setf start end)))))))
        (nreverse result))))

(defun wrap-line (line width)
  "Wrap LINE to fit within WIDTH columns with hanging indent.
   Continuation lines are indented to align with the message text
   after the '[timestamp] nick: ' prefix."
  (when (<= width 0) (return-from wrap-line (list "")))
  (let* ((clean (substitute #\Space #\Newline line))
         (indent (find-message-indent clean)))
    (if (<= (length clean) width)
        (list clean)
        (let* ((pad (make-string indent :initial-element #\Space))
               (cont-width (- width indent)))
          (if (<= cont-width 4)
              ;; Indent too large for useful wrapping, fall back to plain wrap
              (wrap-text clean width)
              ;; Wrap: first line at full width, continuations at reduced width
              (let ((result nil)
                    (start 0)
                    (len (length clean))
                    (first-p t))
                (loop while (< start len) do
                  (let* ((w (if first-p width cont-width))
                         (end (min (+ start w) len)))
                    (if (>= end len)
                        ;; Last chunk
                        (progn
                          (push (if first-p
                                    (subseq clean start end)
                                    (concatenate 'string pad (subseq clean start end)))
                                result)
                          (setf start end))
                        ;; Find word boundary
                        (let ((break-pos (position #\Space clean :start start :end end :from-end t)))
                          (if (and break-pos (> break-pos start))
                              (progn
                                (push (if first-p
                                          (subseq clean start break-pos)
                                          (concatenate 'string pad (subseq clean start break-pos)))
                                      result)
                                (setf start (1+ break-pos)))
                              (progn
                                (push (if first-p
                                          (subseq clean start end)
                                          (concatenate 'string pad (subseq clean start end)))
                                      result)
                                (setf start end)))))
                    (setf first-p nil)))
                (nreverse result)))))))

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
      (let* ((lines (buffer-lines buf))  ; Newest-first (pushed)
             (typing-line-reserved (if typing-str 1 0))
             (max-rows (max 0 (- (rh rect) 2 typing-line-reserved)))
             (max-cols (max 0 (- (rw rect) 2)))
             ;; Build display rows: newest message at top, wrapped lines
             ;; in reading order below their message.
             ;; display-rows will be in top-down render order.
             (display-rows nil)
             (row-colors nil)
             (total 0))
        ;; Walk messages newest-first, wrap each, append in reading order
        (dolist (line lines)
          (when (>= total max-rows) (return))
          (let* ((clean (substitute #\Space #\Newline line))
                 (nick (parse-nick-from-line clean))
                 (color (nick-color nick))
                 (wrapped (wrap-line clean max-cols)))
            ;; Append wrapped lines in reading order (first part, then continuation)
            (dolist (wl wrapped)
              (when (>= total max-rows) (return))
              (push (cons wl color) display-rows)
              (incf total))))
        ;; display-rows is now in reverse order (last pushed = first message's first line)
        ;; Reverse to get top-down render order
        (setf display-rows (nreverse display-rows))
        ;; Render rows top-down
        (loop for i from 0
              for entry in display-rows do
          (setf (de.anvi.croatoan:cursor-position scr)
                (list (+ (ry rect) 1 i) (+ (rx rect) 1)))
          (de.anvi.croatoan:add-string scr (car entry) :fgcolor (cdr entry)))
        ;; Show typing indicator at bottom of chat area
        (when typing-str
          (let ((typing-y (+ (ry rect) (- (rh rect) 2)))
                (typing-display (subseq typing-str 0 (min (length typing-str) max-cols))))
            (setf (de.anvi.croatoan:cursor-position scr) (list typing-y (+ (rx rect) 1)))
            (de.anvi.croatoan:add-string scr typing-display :fgcolor :yellow)))))))

(defmethod handle-input ((w chat-widget) key st ly ui)
  (declare (ignore w key st ly ui))
  nil)
