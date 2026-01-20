;;;; input.lisp - Input line widget for CLabber

(in-package #:clabber)

(defclass input-widget (widget) ())

(defmethod render ((w input-widget) scr rect st ly ui)
  (declare (ignore w st ly))
  (setf (de.anvi.croatoan:cursor-position scr) (list (ry rect) (rx rect)))
  (let* ((prompt "> ")
         (text (ui-input-text ui))
         (max-w (- (rw rect) (length prompt) 1))
         (display (if (> (length text) max-w)
                      (subseq text (- (length text) max-w))
                      text)))
    (de.anvi.croatoan:add-string scr (format nil "~a~a" prompt display))))

(defun input-clear (ui)
  "Clear input text."
  (setf (ui-input-text ui) "")
  (setf (ui-input-cursor ui) 0))

(defun input-add-to-history (ui text)
  "Add TEXT to input history."
  (when (and text (> (length text) 0))
    (push text (ui-input-history ui)))
  (setf (ui-history-index ui) -1))

(defmethod handle-input ((w input-widget) key st ly ui)
  (declare (ignore w st ly))
  ;; Croatoan returns integers from ncurses, convert to char if printable
  (let ((ch (if (integerp key)
                (and (< 31 key 127) (code-char key))
                key)))
    (cond
      ;; Backspace (127 = DEL, 8 = BS, 263 = KEY_BACKSPACE, or character)
      ((or (eql key 127) (eql key 8) (eql key 263)
           (eql key #\Backspace) (eql key #\Rubout))
       (let ((text (ui-input-text ui)))
         (when (> (length text) 0)
           (setf (ui-input-text ui) (subseq text 0 (1- (length text)))))))

      ;; Ctrl-U: clear line
      ((eql key +key-ctrl-u+)
       (input-clear ui))

      ;; Ctrl-A: beginning of line (TODO: cursor movement)
      ((eql key +key-ctrl-a+)
       nil)

      ;; Ctrl-E: end of line (TODO: cursor movement)
      ((eql key +key-ctrl-e+)
       nil)

      ;; Regular printable character
      ((and ch (characterp ch) (graphic-char-p ch))
       (let ((was-empty (zerop (length (ui-input-text ui)))))
         (setf (ui-input-text ui)
               (concatenate 'string (ui-input-text ui) (string ch)))
         ;; Mark that we should send composing state
         (when was-empty
           (setf (ui-composing-p ui) t))))))
  nil)
