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

(defun tab-reset (ui)
  "Reset tab completion state."
  (setf (ui-tab-candidates ui) nil
        (ui-tab-index ui) 0
        (ui-tab-prefix ui) ""
        (ui-tab-word ui) ""))

(defun collect-completions (st ly)
  "Collect nick/JID candidates for tab completion from current context."
  (let* ((candidates nil)
         (chat-pane (focused-chat-pane ly))
         (buf-id (pane-buffer-id ly chat-pane)))
    ;; MUC participants (if current buffer is a MUC)
    (when buf-id
      (let ((buf (find-buffer st buf-id)))
        (when buf
          (maphash (lambda (nick show)
                     (declare (ignore show))
                     (push nick candidates))
                   (buffer-participants buf)))))
    ;; Roster names and JIDs
    (dolist (item (state-roster st))
      (let ((name (roster-name item))
            (jid (roster-jid item)))
        (when name (pushnew name candidates :test #'equal))
        (when jid (pushnew jid candidates :test #'equal))))
    (sort candidates #'string-lessp)))

(defun input-last-word (text)
  "Return (values prefix partial-word) splitting TEXT at the last space.
   PREFIX includes the space; PARTIAL-WORD is the incomplete word being typed."
  (let ((space-pos (position #\Space text :from-end t)))
    (if space-pos
        (values (subseq text 0 (1+ space-pos))
                (subseq text (1+ space-pos)))
        (values "" text))))

(defun tab-complete (st ly ui)
  "Perform tab completion on the current input."
  (let ((text (ui-input-text ui)))
    (if (ui-tab-candidates ui)
        ;; Cycle to next candidate
        (let* ((idx (mod (1+ (ui-tab-index ui)) (length (ui-tab-candidates ui))))
               (candidate (nth idx (ui-tab-candidates ui)))
               (suffix (if (string= (ui-tab-prefix ui) "") ": " " ")))
          (setf (ui-tab-index ui) idx)
          (setf (ui-input-text ui)
                (concatenate 'string (ui-tab-prefix ui) candidate suffix)))
        ;; First Tab press - build candidates
        (multiple-value-bind (prefix partial) (input-last-word text)
          (when (> (length partial) 0)
            (let* ((all (collect-completions st ly))
                   (matches (remove-if-not 
                             (lambda (c) (str:starts-with-p partial c :ignore-case t))
                             all)))
              (when matches
                (setf (ui-tab-prefix ui) prefix
                      (ui-tab-word ui) partial
                      (ui-tab-candidates ui) matches
                      (ui-tab-index ui) 0)
                (let* ((candidate (first matches))
                       (suffix (if (string= prefix "") ": " " ")))
                  (setf (ui-input-text ui)
                        (concatenate 'string prefix candidate suffix))))))))))

(defmethod handle-input ((w input-widget) key st ly ui)
  (declare (ignore w))
  ;; Croatoan returns integers from ncurses, convert to char if printable
  (let ((ch (if (integerp key)
                (and (< 31 key 127) (code-char key))
                key))
        (is-tab (or (eql key +key-tab+) (eql key #\Tab) (eql key 9))))
    (cond
      ;; Tab completion
      (is-tab
       (handler-case (tab-complete st ly ui)
         (error () nil)))

      ;; Backspace (127 = DEL, 8 = BS, 263 = KEY_BACKSPACE, or character)
      ((or (eql key 127) (eql key 8) (eql key 263)
           (eql key #\Backspace) (eql key #\Rubout))
       (tab-reset ui)
       (let ((text (ui-input-text ui)))
         (when (> (length text) 0)
           (setf (ui-input-text ui) (subseq text 0 (1- (length text)))))))

      ;; Ctrl-U: clear line
      ((eql key +key-ctrl-u+)
       (tab-reset ui)
       (input-clear ui))

      ;; Ctrl-A: beginning of line (TODO: cursor movement)
      ((eql key +key-ctrl-a+)
       nil)

      ;; Ctrl-E: end of line (TODO: cursor movement)
      ((eql key +key-ctrl-e+)
       nil)

      ;; Regular printable character
      ((and ch (characterp ch) (graphic-char-p ch))
       (tab-reset ui)
       (let ((was-empty (zerop (length (ui-input-text ui)))))
         (setf (ui-input-text ui)
               (concatenate 'string (ui-input-text ui) (string ch)))
         ;; Mark that we should send composing state
         (when was-empty
           (setf (ui-composing-p ui) t))))))
  nil)
