;;;; bufferbar.lisp - Buffer bar widget for CLabber

(in-package #:clabber)

(defclass bufferbar-widget (widget) ())

(defun short-buffer-name (id)
  "Extract short display name from buffer ID (JID).
   For MUC rooms like #channel%server@bridge, return just #channel.
   For contacts, return the username part."
  (let ((name (princ-to-string id)))
    (cond
      ;; MUC via IRC bridge: #channel%server@bridge -> #channel
      ((and (> (length name) 0) (char= (char name 0) #\#))
       (let ((percent-pos (position #\% name)))
         (if percent-pos
             (subseq name 0 percent-pos)
             ;; No %, try @ for regular MUC
             (let ((at-pos (position #\@ name)))
               (if at-pos (subseq name 0 at-pos) name)))))
      ;; Regular JID: user@domain -> user
      (t (let ((at-pos (position #\@ name)))
           (if at-pos (subseq name 0 at-pos) name))))))

(defun format-buffer-pill (id &key focused unread pane-label)
  "Format a buffer indicator pill."
  (let ((name (short-buffer-name id)))
    (format nil "[~a~a~a~a]"
            (if pane-label (format nil "~a:" pane-label) "")
            name
            (if (and unread (> unread 0)) (format nil " ~d" unread) "")
            (if focused "*" ""))))

(defmethod render ((w bufferbar-widget) scr rect st ly ui)
  (declare (ignore w ui))
  (let* ((theme (current-theme))
         (open (open-buffers-list st))
         (a (layout-chat-a-buffer ly))
         (b (layout-chat-b-buffer ly))
         (focus (focused-chat-pane ly)))
    (setf (de.anvi.croatoan:cursor-position scr) (list (ry rect) (rx rect)))
    (de.anvi.croatoan:add-string scr "Buffers: ")

    (let ((x (+ (rx rect) (length "Buffers: ")))
          (maxx (+ (rx rect) (rw rect))))
      (labels ((emit (s &optional color)
                 (when (< (+ x (length s) 1) maxx)
                   (setf (de.anvi.croatoan:cursor-position scr) (list (ry rect) x))
                   (de.anvi.croatoan:add-string scr s :fgcolor color)
                   (incf x (+ (length s) 1))
                   t)))
        (let ((bufa (state-get-buffer st a))
              (bufb (and (layout-split-enabled-p ly) (state-get-buffer st b))))
          ;; Active buffer in cyan, with unread in yellow
          (let* ((unread-a (if bufa (buffer-unread bufa) 0))
                 (color-a (cond ((eql focus :chat-a) (theme-active-buffer-fg theme))
                                ((> unread-a 0) (theme-unread-fg theme))
                                (t nil))))
            (emit (format-buffer-pill a :pane-label "A"
                                      :focused (eql focus :chat-a)
                                      :unread unread-a)
                  color-a))
          (when (layout-split-enabled-p ly)
            (let* ((unread-b (if bufb (buffer-unread bufb) 0))
                   (color-b (cond ((eql focus :chat-b) (theme-active-buffer-fg theme))
                                  ((> unread-b 0) (theme-unread-fg theme))
                                  (t nil))))
              (emit (format-buffer-pill b :pane-label "B"
                                        :focused (eql focus :chat-b)
                                        :unread unread-b)
                    color-b)))

          (let ((n 1))
            (dolist (id open)
              (unless (or (equal id a)
                          (and (layout-split-enabled-p ly) (equal id b)))
                (let* ((buf (state-get-buffer st id))
                       (unread (if buf (buffer-unread buf) 0))
                       (color (if (> unread 0) (theme-unread-fg theme) nil)))
                  (emit (format nil "[~d:~a~a]"
                                n
                                (short-buffer-name id)
                                (if (> unread 0)
                                    (format nil " ~d" unread)
                                    ""))
                        color))
                (incf n)
                (when (> n 9) (return))))))

        (when (< x maxx)
          (setf (de.anvi.croatoan:cursor-position scr) (list (ry rect) (1- maxx)))
          (de.anvi.croatoan:add-string scr " "))))))

(defmethod handle-input ((w bufferbar-widget) key st ly ui)
  (declare (ignore w key st ly ui))
  nil)
