;;;; status.lisp - Status bar widget for CLabber

(in-package #:clabber)

(defclass status-widget (widget) ())

(defmethod render ((w status-widget) scr rect st ly ui)
  (declare (ignore w ui))
  (setf (de.anvi.croatoan:cursor-position scr) (list (ry rect) (rx rect)))
  (let* ((theme (current-theme))
         (status-str (state-status st))
         (split-str (if (layout-split-enabled-p ly) "on" "off"))
         (orient-str (symbol-name (layout-split-orientation ly)))
         (focus-str (symbol-name (layout-focused-pane ly)))
         (line (format nil " ~a | Split:~a (~a) | Focus:~a | C-w:split C-t:orient Tab:focus C-q:quit"
                       status-str split-str orient-str focus-str))
         (max-w (rw rect))
         (color (theme-status-fg theme)))
    (de.anvi.croatoan:add-string scr (subseq line 0 (min (length line) max-w)) :fgcolor color)))

(defmethod handle-input ((w status-widget) key st ly ui)
  (declare (ignore w key st ly ui))
  nil)
