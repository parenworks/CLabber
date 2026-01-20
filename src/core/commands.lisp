;;;; commands.lisp - Command classes and execute methods for CLabber

(in-package #:clabber)

(defclass command () ())

(defclass toggle-split (command) ())

(defclass toggle-orientation (command) ())

(defclass cycle-focus (command) ())

(defclass roster-open-selection (command) ())

(defclass roster-move (command)
  ((dir :initarg :dir :reader cmd-dir)))

(defclass cycle-buffer (command)
  ((dir :initarg :dir :reader cmd-dir)))

(defclass close-current-buffer (command) ())

(defclass jump-open-buffer (command)
  ((index :initarg :index :reader cmd-index)))

(defclass send-message (command)
  ((to :initarg :to :reader cmd-to)
   (body :initarg :body :reader cmd-body)))

(defclass quit-app (command) ())

(defclass swap-panes (command) ())

(defgeneric execute (cmd st ly engine)
  (:documentation "Execute command, returning updated state and layout."))

(defmethod execute ((c toggle-split) (st app-state) (ly layout) engine)
  (declare (ignore c engine))
  (if (layout-split-enabled-p ly)
      (progn
        (setf (layout-split-enabled-p ly) nil
              (layout-chat-b-buffer ly) nil)
        (when (eql (layout-focused-pane ly) :chat-b)
          (setf (layout-focused-pane ly) :chat-a)))
      (progn
        (setf (layout-split-enabled-p ly) t)
        (setf (layout-chat-b-buffer ly) (or (car (last (open-buffers-list st))) :system))))
  (note-chat-focus ly)
  (values st ly))

(defmethod execute ((c toggle-orientation) (st app-state) (ly layout) engine)
  (declare (ignore c engine))
  (setf (layout-split-orientation ly)
        (if (eql (layout-split-orientation ly) :vertical) :horizontal :vertical))
  (values st ly))

(defmethod execute ((c cycle-focus) (st app-state) (ly layout) engine)
  (declare (ignore c st engine))
  (setf (layout-focused-pane ly)
        (case (layout-focused-pane ly)
          (:roster :chat-a)
          (:chat-a (if (layout-split-enabled-p ly) :chat-b :roster))
          (:chat-b :roster)
          (t :chat-a)))
  (note-chat-focus ly)
  (values st ly))

(defmethod execute ((c roster-move) (st app-state) (ly layout) engine)
  (declare (ignore engine))
  (let ((n (roster-count st)))
    (when (> n 0)
      (setf (layout-roster-index ly)
            (clamp (+ (layout-roster-index ly) (cmd-dir c)) 0 (1- n)))))
  (values st ly))

(defmethod execute ((c roster-open-selection) (st app-state) (ly layout) engine)
  (declare (ignore c engine))
  (let ((jid (roster-selected-jid st ly)))
    (when jid
      (state-ensure-buffer st jid :title jid)
      (open-buffer! st jid)
      (touch-open-buffer! st jid)
      (setf (pane-buffer-id ly (target-chat-pane ly)) jid)
      (mark-visible-read! st jid)))
  (values st ly))

(defun cycle-open-buffer-id (st current-id dir)
  "Cycle through open buffers in direction DIR."
  (let ((ids (open-buffers-list st)))
    (cond
      ((null ids) :system)
      (t
       (let* ((pos (position current-id ids :test #'equal))
              (len (length ids))
              (start (or pos 0))
              (next (mod (+ start dir) len)))
         (nth next ids))))))

(defmethod execute ((c cycle-buffer) (st app-state) (ly layout) engine)
  (declare (ignore engine))
  (let* ((pane (focused-chat-pane ly))
         (cur (pane-buffer-id ly pane))
         (next (cycle-open-buffer-id st cur (cmd-dir c))))
    (setf (pane-buffer-id ly pane) next)
    (touch-open-buffer! st next)
    (mark-visible-read! st next))
  (values st ly))

(defun next-buffer-after-close (st closed-id)
  "Return next buffer to show after closing CLOSED-ID."
  (let* ((open (remove closed-id (open-buffers-list st) :test #'equal))
         (candidate (car (last open))))
    (or candidate :system)))

(defmethod execute ((c close-current-buffer) (st app-state) (ly layout) engine)
  (declare (ignore c engine))
  (let* ((pane (focused-chat-pane ly))
         (cur (pane-buffer-id ly pane)))
    (unless (eql cur :system)
      (close-buffer! st cur)
      (let ((next (next-buffer-after-close st cur)))
        (setf (pane-buffer-id ly pane) next)
        (mark-visible-read! st next))))
  (values st ly))

(defun nth-open-buffer-id (st n)
  "Return Nth open buffer ID (1-indexed)."
  (let* ((ids (open-buffers-list st))
         (i (1- n)))
    (when (and (>= i 0) (< i (length ids)))
      (nth i ids))))

(defmethod execute ((c jump-open-buffer) (st app-state) (ly layout) engine)
  (declare (ignore engine))
  (let* ((pane (focused-chat-pane ly))
         (id (nth-open-buffer-id st (cmd-index c))))
    (when id
      (setf (pane-buffer-id ly pane) id)
      (touch-open-buffer! st id)
      (mark-visible-read! st id)))
  (values st ly))

(defmethod execute ((c send-message) (st app-state) (ly layout) engine)
  (let* ((to (cmd-to c))
         (body (cmd-body c))
         (buf (state-ensure-buffer st to :title to))
         ;; Check if target is a MUC room by looking at roster
         (roster-item (find to (state-roster st) :key #'roster-jid :test #'string=))
         (is-muc (and roster-item (string= (roster-presence roster-item) "muc")))
         (timestamp (format-timestamp (current-timestamp))))
    (buffer-add-line buf (format nil "~@[~a ~]me: ~a" timestamp body))
    (if is-muc
        (engine-send-groupchat engine to body)
        (engine-send-message engine to body)))
  (values st ly))

(defmethod execute ((c quit-app) (st app-state) (ly layout) engine)
  (declare (ignore c ly engine))
  (setf (state-running-p st) nil)
  (values st ly))

(defmethod execute ((c swap-panes) (st app-state) (ly layout) engine)
  (declare (ignore c engine))
  (when (layout-split-enabled-p ly)
    (let ((a (layout-chat-a-buffer ly))
          (b (layout-chat-b-buffer ly)))
      (setf (layout-chat-a-buffer ly) b
            (layout-chat-b-buffer ly) a)))
  (values st ly))

(defmethod execute ((c command) (st app-state) (ly layout) engine)
  (declare (ignore c ly engine))
  (values st ly))
