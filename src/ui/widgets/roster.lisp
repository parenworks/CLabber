;;;; roster.lisp - Roster widget for CLabber

(in-package #:clabber)

(defclass roster-widget (widget) ())

(defun draw-box (scr rect title)
  "Draw a box with optional title using theme colors."
  (let* ((theme (current-theme))
         (color (theme-border-fg theme))
         (w (rw rect))
         (h (rh rect)))
    ;; Draw top border: +----+
    (setf (de.anvi.croatoan:cursor-position scr) (list (ry rect) (rx rect)))
    (de.anvi.croatoan:add-string scr 
      (concatenate 'string "+" (make-string (- w 2) :initial-element #\-) "+")
      :fgcolor color)
    ;; Draw bottom border: +----+
    (setf (de.anvi.croatoan:cursor-position scr) (list (+ (ry rect) (1- h)) (rx rect)))
    (de.anvi.croatoan:add-string scr 
      (concatenate 'string "+" (make-string (- w 2) :initial-element #\-) "+")
      :fgcolor color)
    ;; Draw side borders: |
    (loop for y from 1 below (1- h) do
      (setf (de.anvi.croatoan:cursor-position scr) (list (+ (ry rect) y) (rx rect)))
      (de.anvi.croatoan:add-string scr "|" :fgcolor color)
      (setf (de.anvi.croatoan:cursor-position scr) (list (+ (ry rect) y) (+ (rx rect) (1- w))))
      (de.anvi.croatoan:add-string scr "|" :fgcolor color))
    ;; Draw title
    (when title
      (setf (de.anvi.croatoan:cursor-position scr) (list (ry rect) (+ (rx rect) 2)))
      (de.anvi.croatoan:add-string scr title :fgcolor color))))

(defun presence-indicator (presence)
  "Return a character indicating presence status."
  (cond
    ((string= presence "available") "+")
    ((string= presence "away") "~")
    ((string= presence "dnd") "-")
    ((string= presence "xa") ".")
    ((string= presence "muc") "#")
    (t " ")))

(defmethod render ((w roster-widget) scr rect st ly ui)
  (declare (ignore w ui))
  (let ((title (if (eql (layout-focused-pane ly) :roster) "Roster *" "Roster"))
        (theme (current-theme)))
    (draw-box scr rect title)
    (let* ((items (state-roster st))
           (n (length items))
           (sel (clamp (layout-roster-index ly) 0 (max 0 (1- n))))
           (max-lines (max 0 (- (rh rect) 2)))
           (max-cols (max 0 (- (rw rect) 4))))
      (loop for i from 0 below (min max-lines n) do
        (let* ((it (nth i items))
               (jid (roster-jid it))
               (name (or (roster-name it) jid))
               (presence (roster-presence it))
               (pres (presence-indicator presence))
               (mark (if (= i sel) ">" " "))
               (display (if (> (length name) max-cols)
                            (subseq name 0 max-cols)
                            name))
               ;; Use theme colors
               (color (if (= i sel)
                          (theme-roster-selected-fg theme)
                          (theme-presence-color theme presence))))
          (setf (de.anvi.croatoan:cursor-position scr) (list (+ (ry rect) 1 i) (+ (rx rect) 1)))
          (de.anvi.croatoan:add-string scr (format nil "~a~a ~a" mark pres display) :fgcolor color))))))

(defmethod handle-input ((w roster-widget) key st ly ui)
  (declare (ignore w st ui))
  (when (and (eql (layout-focused-pane ly) :roster)
             (or (eql key #\Newline) (eql key #\Return)))
    (list (make-instance 'roster-open-selection))))
