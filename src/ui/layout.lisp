;;;; layout.lisp - Layout computation for CLabber

(in-package #:clabber)

(defclass rect ()
  ((x :initarg :x :reader rx)
   (y :initarg :y :reader ry)
   (w :initarg :w :reader rw)
   (h :initarg :h :reader rh)))

(defun make-rect (&key x y w h)
  "Create a rectangle."
  (make-instance 'rect :x x :y y :w w :h h))

(defun compute-frames (scr ly)
  "Return plist of rects: :roster :bufferbar :chat-a :chat-b :status :input
Layout like CLatter: input at row 0, status at row 1, chat starts at row 2,
bufferbar at bottom. Roster on left full height."
  (let* ((rows (de.anvi.croatoan:height scr))
         (cols (de.anvi.croatoan:width scr))
         (input-h 1)
         (status-h 1)
         (bufferbar-h 1)
         (roster-w (clamp (layout-roster-width ly) 20 (max 20 (floor cols 2))))
         (chat-x roster-w)
         (chat-w (- cols roster-w))
         ;; Layout: input at row 0, status at row 1, chat starts at row 2
         (chat-y 2)
         (chat-h (- rows chat-y bufferbar-h))
         ;; Roster on left, full height
         (roster (make-rect :x 0 :y 0 :w roster-w :h rows))
         ;; Input at top right (row 0)
         (input (make-rect :x chat-x :y 0 :w chat-w :h input-h))
         ;; Status below input (row 1)
         (status (make-rect :x chat-x :y 1 :w chat-w :h status-h))
         ;; Buffer bar at very bottom
         (bufferbar (make-rect :x chat-x :y (- rows bufferbar-h) :w chat-w :h bufferbar-h)))
    (cond
      ((not (layout-split-enabled-p ly))
       (list :roster roster
             :bufferbar bufferbar
             :chat-a (make-rect :x chat-x :y chat-y :w chat-w :h chat-h)
             :chat-b nil
             :status status
             :input input))
      ((eql (layout-split-orientation ly) :horizontal)
       ;; Horizontal split: two panes side by side
       (let ((w1 (max 10 (floor chat-w 2)))
             (w2 (- chat-w (max 10 (floor chat-w 2)))))
         (list :roster roster
               :bufferbar bufferbar
               :chat-a (make-rect :x chat-x :y chat-y :w w1 :h chat-h)
               :chat-b (make-rect :x (+ chat-x w1) :y chat-y :w w2 :h chat-h)
               :status status
               :input input)))
      (t
       ;; Vertical split: two panes stacked
       (let ((h1 (max 5 (floor chat-h 2)))
             (h2 (- chat-h (max 5 (floor chat-h 2)))))
         (list :roster roster
               :bufferbar bufferbar
               :chat-a (make-rect :x chat-x :y chat-y :w chat-w :h h1)
               :chat-b (make-rect :x chat-x :y (+ chat-y h1) :w chat-w :h h2)
               :status status
               :input input))))))
