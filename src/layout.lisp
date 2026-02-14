(in-package #:clabber.layout)

;;; Layout Manager for CLabber UI
;;; Computes panel positions and dimensions based on terminal size

(defclass layout ()
  ((roster-width :initarg :roster-width :accessor layout-roster-width :initform 28)
   (participants-width :initarg :participants-width :accessor layout-participants-width :initform 22)
   (split-mode :initarg :split-mode :accessor layout-split-mode :initform nil
               :documentation "Split mode: nil (single), :horizontal, :vertical")
   ;; Computed panel instances
   (input :accessor layout-input :initform nil)
   (status :accessor layout-status :initform nil)
   (roster :accessor layout-roster :initform nil)
   (chat-a :accessor layout-chat-a :initform nil)
   (chat-b :accessor layout-chat-b :initform nil)
   (participants :accessor layout-participants :initform nil)
   (buffer-bar :accessor layout-buffer-bar :initform nil))
  (:documentation "Screen layout manager"))

(defun make-layout (&key (roster-width 28) (participants-width 22) split-mode)
  (make-instance 'layout
                 :roster-width roster-width
                 :participants-width participants-width
                 :split-mode split-mode))

(defun layout-compute (layout width height)
  "Compute panel positions and dimensions for the given terminal size.
   Layout: input at row 1, status at row 2, chat starts at row 3,
   roster on left full height, participants on right, buffer bar at bottom."
  (let* ((roster-w (min (layout-roster-width layout) (floor width 3)))
         (participants-w (layout-participants-width layout))
         (chat-x (1+ roster-w))
         (chat-w (- width roster-w))  ; total space after roster (includes participants)
         (input-y 1)
         (status-y 2)
         (chat-y 3)
         (bufbar-h 1)
         (chat-h (- height 2 bufbar-h))  ; subtract input + status + bufbar
         (split (layout-split-mode layout)))
    ;; Input bar at top (row 1), spans chat + participants width
    (setf (layout-input layout)
          (make-instance 'input-bar
                         :x chat-x :y input-y
                         :width (- width roster-w) :height 1))
    ;; Status bar (row 2)
    (setf (layout-status layout)
          (make-instance 'status-bar
                         :x chat-x :y status-y
                         :width (- width roster-w) :height 1))
    ;; Roster panel (full height on left)
    (setf (layout-roster layout)
          (make-instance 'roster-panel
                         :x 1 :y 1
                         :width roster-w :height height))
    ;; Participants panel (right side, from chat-y to bottom)
    (let ((chat-area-w (- chat-w participants-w)))
      (setf (layout-participants layout)
            (make-instance 'participants-panel
                           :x (+ chat-x chat-area-w) :y chat-y
                           :width participants-w :height chat-h))
      ;; Buffer bar at bottom
      (setf (layout-buffer-bar layout)
            (make-instance 'buffer-bar
                           :x chat-x :y height
                           :width chat-w :height 1))
      ;; Chat panel(s) based on split mode
      (cond
        ;; Horizontal split: two panes side by side
        ((eq split :horizontal)
         (let* ((pane-w (floor (- chat-area-w 1) 2))
                (pane2-x (+ chat-x pane-w 1)))
           (setf (layout-chat-a layout)
                 (make-instance 'chat-panel
                                :x chat-x :y chat-y
                                :width pane-w :height chat-h))
           (setf (layout-chat-b layout)
                 (make-instance 'chat-panel
                                :x pane2-x :y chat-y
                                :width pane-w :height chat-h))))
        ;; Vertical split: two panes stacked
        ((eq split :vertical)
         (let* ((pane-h (floor (- chat-h 1) 2))
                (pane2-y (+ chat-y pane-h 1)))
           (setf (layout-chat-a layout)
                 (make-instance 'chat-panel
                                :x chat-x :y chat-y
                                :width chat-area-w :height pane-h))
           (setf (layout-chat-b layout)
                 (make-instance 'chat-panel
                                :x chat-x :y pane2-y
                                :width chat-area-w :height pane-h))))
        ;; Single pane
        (t
         (setf (layout-chat-a layout)
               (make-instance 'chat-panel
                              :x chat-x :y chat-y
                              :width chat-area-w :height chat-h))
         (setf (layout-chat-b layout) nil))))
    layout))

(defun layout-render-all (layout)
  "Render all panels in the layout."
  (begin-sync-update)
  (when (layout-roster layout) (panel-render (layout-roster layout)))
  (when (layout-chat-a layout) (panel-render (layout-chat-a layout)))
  (when (layout-chat-b layout) (panel-render (layout-chat-b layout)))
  (when (layout-participants layout) (panel-render (layout-participants layout)))
  (when (layout-buffer-bar layout) (panel-render (layout-buffer-bar layout)))
  (when (layout-status layout) (panel-render (layout-status layout)))
  (when (layout-input layout) (panel-render (layout-input layout)))
  (end-sync-update)
  (force-output *terminal-io*))
