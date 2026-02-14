(in-package #:clabber.model)

;;; Data Model for CLabber
;;; Buffers, messages, contacts, MUC state

;;; Message class

(defclass message ()
  ((text :initarg :text :accessor message-text)
   (nick :initarg :nick :accessor message-nick :initform nil)
   (timestamp :initarg :timestamp :accessor message-timestamp
              :initform (get-universal-time))
   (level :initarg :level :accessor message-level :initform nil
          :documentation "Message level: nil (normal), :join, :part, :error, :system, :presence")
   (highlight-p :initarg :highlight :accessor message-highlight-p :initform nil
                :documentation "Whether this message mentions us"))
  (:documentation "A single chat message"))

(defun make-message (&key text nick timestamp level highlight)
  (make-instance 'message
                 :text text :nick nick
                 :timestamp (or timestamp (get-universal-time))
                 :level level :highlight highlight))

;;; Buffer class - represents a chat window (DM, MUC, or system)

(defclass buffer ()
  ((name :initarg :name :accessor buffer-name
         :documentation "Full JID or identifier")
   (display-name :initarg :display-name :accessor buffer-display-name :initform nil
                 :documentation "Short display name (e.g. stripped MUC name)")
   (type :initarg :type :accessor buffer-type :initform :dm
         :documentation "Buffer type: :dm, :muc, :system")
   (messages :initform (make-array 1000 :adjustable t :fill-pointer 0)
             :accessor buffer-messages
             :documentation "Ring buffer of messages")
   (unread-count :initform 0 :accessor buffer-unread-count)
   (mention-p :initform nil :accessor buffer-mention-p
              :documentation "Whether there's an unread mention")
   (participants :initform nil :accessor buffer-participants
                 :documentation "List of participant nicks (for MUCs)")
   (scroll-offset :initform 0 :accessor buffer-scroll-offset)
   (omemo-p :initform nil :accessor buffer-omemo-p
            :documentation "Whether OMEMO encryption is active"))
  (:documentation "A chat buffer (DM, MUC, or system)"))

(defun make-buffer (&key name display-name (type :dm))
  (make-instance 'buffer :name name :display-name display-name :type type))

(defun buffer-add-message (buffer message)
  "Add a message to the buffer, incrementing unread count."
  (vector-push-extend message (buffer-messages buffer))
  (incf (buffer-unread-count buffer))
  (when (message-highlight-p message)
    (setf (buffer-mention-p buffer) t))
  message)

(defun buffer-message-count (buffer)
  (fill-pointer (buffer-messages buffer)))

(defun buffer-last-message (buffer)
  (let ((count (buffer-message-count buffer)))
    (when (> count 0)
      (aref (buffer-messages buffer) (1- count)))))

(defun strip-muc-name (full-jid)
  "Strip MUC JID to IRC-style short name.
   e.g. 'room%irc.libera.chat@biboumi.example.com' -> '#room'
   e.g. 'room@conference.example.com' -> '#room'"
  (let* ((local-part (subseq full-jid 0 (position #\@ full-jid)))
         (stripped (if (position #\% local-part)
                       (subseq local-part 0 (position #\% local-part))
                       local-part)))
    (if (char= (char stripped 0) #\#)
        stripped
        (concatenate 'string "#" stripped))))

;;; Contact class

(defclass contact ()
  ((jid :initarg :jid :accessor contact-jid)
   (nick :initarg :nick :accessor contact-nick :initform nil)
   (presence :initarg :presence :accessor contact-presence :initform :offline
             :documentation "Presence: :online, :away, :dnd, :xa, :offline")
   (subscription :initarg :subscription :accessor contact-subscription :initform nil))
  (:documentation "A roster contact"))

(defun make-contact (&key jid nick presence subscription)
  (make-instance 'contact :jid jid :nick nick
                 :presence (or presence :offline)
                 :subscription subscription))
