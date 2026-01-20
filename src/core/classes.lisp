;;;; classes.lisp - Core CLOS classes for CLabber

(in-package #:clabber)

;;; ============================================================
;;; Core Data Classes
;;; ============================================================

(defclass buffer ()
  ((id      :initarg :id :reader buffer-id)
   (title   :initarg :title :accessor buffer-title)
   (lines   :initform '() :accessor buffer-lines)
   (unread  :initform 0   :accessor buffer-unread))
  (:documentation "A chat buffer containing message lines."))

(defclass roster-item ()
  ((jid      :initarg :jid :reader roster-jid)
   (name     :initarg :name :initform nil :accessor roster-name)
   (presence :initarg :presence :initform "offline" :accessor roster-presence))
  (:documentation "A contact in the roster."))

(defclass app-state ()
  ((running     :initform t :accessor state-running-p)
   (connected   :initform nil :accessor state-connected-p)
   (status      :initform "offline" :accessor state-status)
   (my-jid      :initform nil :accessor state-my-jid)
   (my-nick     :initform nil :accessor state-my-nick)
   (buffers     :initform (list (make-instance 'buffer :id :system :title "System"))
               :accessor state-buffers)
   (roster      :initform '()
               :accessor state-roster)
   (open-buffer-ids :initform '() :accessor state-open-buffer-ids)
   (auto-open-on-message :initform nil :accessor state-auto-open-on-message-p))
  (:documentation "Application state containing buffers, roster, and connection status."))

(defclass layout ()
  ((roster-width      :initform 28 :accessor layout-roster-width)
   (split-enabled     :initform nil :accessor layout-split-enabled-p)
   (split-orientation :initform :vertical :accessor layout-split-orientation)
   (focused-pane      :initform :chat-a :accessor layout-focused-pane)
   (last-chat-focus   :initform :chat-a :accessor layout-last-chat-focus)
   (chat-a-buffer     :initform :system :accessor layout-chat-a-buffer)
   (chat-b-buffer     :initform nil :accessor layout-chat-b-buffer)
   (roster-index      :initform 0 :accessor layout-roster-index))
  (:documentation "UI layout state including split configuration and focus."))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun clamp (v lo hi)
  "Clamp value V between LO and HI."
  (max lo (min hi v)))

;;; ============================================================
;;; Generic Functions - Buffer Protocol
;;; ============================================================

(defgeneric add-line (target line)
  (:documentation "Add a line of text to TARGET."))

(defgeneric clear-unread (target)
  (:documentation "Clear unread count for TARGET."))

(defgeneric find-buffer (container id)
  (:documentation "Find a buffer by ID in CONTAINER."))

(defgeneric ensure-buffer (container id &key title)
  (:documentation "Get or create a buffer with ID in CONTAINER."))

(defgeneric log-to-buffer (target message)
  (:documentation "Log a message to TARGET's system buffer."))

;;; ============================================================
;;; Generic Functions - Roster Protocol
;;; ============================================================

(defgeneric roster-count (container)
  (:documentation "Return number of roster items in CONTAINER."))

(defgeneric selected-item (container selector)
  (:documentation "Return the currently selected item from CONTAINER using SELECTOR."))

;;; ============================================================
;;; Generic Functions - Layout Protocol
;;; ============================================================

(defgeneric note-focus (layout)
  (:documentation "Record the current focus state in LAYOUT."))

(defgeneric target-pane (layout)
  (:documentation "Return the pane to use for opening buffers."))

(defgeneric focused-pane (layout)
  (:documentation "Return the currently focused chat pane."))

(defgeneric pane-buffer (layout pane)
  (:documentation "Get buffer ID for PANE in LAYOUT."))

(defgeneric (setf pane-buffer) (value layout pane)
  (:documentation "Set buffer ID for PANE in LAYOUT."))

(defgeneric buffer-visible-p (layout buffer-id)
  (:documentation "Return T if BUFFER-ID is visible in any pane."))

;;; ============================================================
;;; Generic Functions - Open Buffers Protocol
;;; ============================================================

(defgeneric open-buffers (container)
  (:documentation "Return list of open buffer IDs."))

(defgeneric buffer-open-p (container id)
  (:documentation "Return T if buffer ID is open."))

(defgeneric open-buffer (container id)
  (:documentation "Open buffer ID (add to open buffers list)."))

(defgeneric close-buffer (container id)
  (:documentation "Close buffer ID (remove from open buffers list)."))

(defgeneric touch-buffer (container id)
  (:documentation "Move buffer ID to end of open buffers list (MRU)."))

;;; ============================================================
;;; Method Implementations - Buffer
;;; ============================================================

(defmethod add-line ((buf buffer) line)
  "Add LINE to buffer BUF."
  (push line (buffer-lines buf))
  buf)

(defmethod clear-unread ((buf buffer))
  "Clear unread count for buffer."
  (setf (buffer-unread buf) 0)
  buf)

;;; ============================================================
;;; Method Implementations - App State
;;; ============================================================

(defmethod find-buffer ((st app-state) id)
  "Find buffer by ID in state ST."
  (find id (state-buffers st) :key #'buffer-id :test #'equal))

(defmethod ensure-buffer ((st app-state) id &key title)
  "Get or create buffer with ID in state ST."
  (or (find-buffer st id)
      (let ((b (make-instance 'buffer :id id :title (or title (princ-to-string id)))))
        (push b (state-buffers st))
        b)))

(defmethod log-to-buffer ((st app-state) message)
  "Log MESSAGE to the system buffer."
  (add-line (ensure-buffer st :system :title "System") message)
  st)

(defmethod roster-count ((st app-state))
  "Return number of roster items."
  (length (state-roster st)))

(defmethod open-buffers ((st app-state))
  "Return list of open buffer IDs."
  (state-open-buffer-ids st))

(defmethod buffer-open-p ((st app-state) id)
  "Return T if buffer ID is open."
  (member id (state-open-buffer-ids st) :test #'equal))

(defmethod open-buffer ((st app-state) id)
  "Open buffer ID (add to open buffers list)."
  (unless (or (eql id :system) (buffer-open-p st id))
    (setf (state-open-buffer-ids st)
          (append (state-open-buffer-ids st) (list id))))
  st)

(defmethod close-buffer ((st app-state) id)
  "Close buffer ID (remove from open buffers list)."
  (setf (state-open-buffer-ids st)
        (remove id (state-open-buffer-ids st) :test #'equal))
  st)

(defmethod touch-buffer ((st app-state) id)
  "Move buffer ID to end of open buffers list (most recently used)."
  (when (and (not (eql id :system)) (buffer-open-p st id))
    (setf (state-open-buffer-ids st)
          (append (remove id (state-open-buffer-ids st) :test #'equal)
                  (list id))))
  st)

;;; ============================================================
;;; Method Implementations - Layout
;;; ============================================================

(defmethod selected-item ((st app-state) (ly layout))
  "Return JID of currently selected roster item."
  (let ((n (roster-count st)))
    (when (> n 0)
      (let* ((idx (clamp (layout-roster-index ly) 0 (1- n)))
             (it (nth idx (state-roster st))))
        (roster-jid it)))))

(defmethod note-focus ((ly layout))
  "Remember which chat pane was last focused."
  (when (member (layout-focused-pane ly) '(:chat-a :chat-b))
    (setf (layout-last-chat-focus ly) (layout-focused-pane ly)))
  ly)

(defmethod target-pane ((ly layout))
  "Return the chat pane to use for opening buffers."
  (let ((p (layout-last-chat-focus ly)))
    (if (and (eql p :chat-b) (layout-split-enabled-p ly)) :chat-b :chat-a)))

(defmethod focused-pane ((ly layout))
  "Return the currently focused chat pane."
  (case (layout-focused-pane ly)
    (:chat-b (if (layout-split-enabled-p ly) :chat-b :chat-a))
    (:chat-a :chat-a)
    (t :chat-a)))

(defmethod pane-buffer ((ly layout) pane)
  "Get buffer ID for PANE."
  (ecase pane
    (:chat-a (layout-chat-a-buffer ly))
    (:chat-b (layout-chat-b-buffer ly))))

(defmethod (setf pane-buffer) (val (ly layout) pane)
  "Set buffer ID for PANE."
  (ecase pane
    (:chat-a (setf (layout-chat-a-buffer ly) val))
    (:chat-b (when (layout-split-enabled-p ly)
               (setf (layout-chat-b-buffer ly) val))))
  val)

(defmethod buffer-visible-p ((ly layout) buffer-id)
  "Return T if BUFFER-ID is visible in any chat pane."
  (or (equal buffer-id (layout-chat-a-buffer ly))
      (and (layout-split-enabled-p ly)
           (equal buffer-id (layout-chat-b-buffer ly)))))

;;; ============================================================
;;; Compatibility Aliases (for existing code)
;;; ============================================================
;;; These maintain backward compatibility during transition.

(defun state-get-buffer (st id)
  "Alias for find-buffer."
  (find-buffer st id))

(defun state-ensure-buffer (st id &key title)
  "Alias for ensure-buffer."
  (ensure-buffer st id :title title))

(defun buffer-add-line (buf line)
  "Alias for add-line."
  (add-line buf line))

(defun state-log (st line)
  "Alias for log-to-buffer."
  (log-to-buffer st line))

(defun roster-selected-jid (st ly)
  "Alias for selected-item."
  (selected-item st ly))

(defun note-chat-focus (ly)
  "Alias for note-focus."
  (note-focus ly))

(defun target-chat-pane (ly)
  "Alias for target-pane."
  (target-pane ly))

(defun focused-chat-pane (ly)
  "Alias for focused-pane."
  (focused-pane ly))

(defun pane-buffer-id (ly pane)
  "Alias for pane-buffer."
  (pane-buffer ly pane))

(defun (setf pane-buffer-id) (val ly pane)
  "Alias for (setf pane-buffer)."
  (setf (pane-buffer ly pane) val))

(defun open-buffers-list (st)
  "Alias for open-buffers."
  (open-buffers st))

(defun open-buffer! (st id)
  "Alias for open-buffer."
  (open-buffer st id))

(defun close-buffer! (st id)
  "Alias for close-buffer."
  (close-buffer st id))

(defun touch-open-buffer! (st id)
  "Alias for touch-buffer."
  (touch-buffer st id))

(defun mark-visible-read! (st buf-id)
  "Clear unread count for buffer in state."
  (let ((buf (find-buffer st buf-id)))
    (when buf (clear-unread buf)))
  st)
