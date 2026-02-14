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
                :documentation "Whether this message mentions us")
   (stanza-id :initarg :stanza-id :accessor message-stanza-id :initform nil
              :documentation "XMPP stanza ID for message correction")
   (edited-p :initarg :edited :accessor message-edited-p :initform nil
             :documentation "Whether this message has been edited"))
  (:documentation "A single chat message"))

(defun make-message (&key text nick timestamp level highlight stanza-id)
  (make-instance 'message
                 :text text :nick nick
                 :timestamp (or timestamp (get-universal-time))
                 :level level :highlight highlight
                 :stanza-id stanza-id))

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
                 :documentation "List of (nick . role) conses for MUCs. Role is :moderator, :participant, :visitor, or :none")
   (topic :initform nil :accessor buffer-topic
          :documentation "MUC topic / subject string")
   (modes :initform nil :accessor buffer-modes
          :documentation "IRC channel modes string (e.g. +Cnt)")
   (scroll-offset :initform 0 :accessor buffer-scroll-offset)
   (omemo-p :initform nil :accessor buffer-omemo-p
            :documentation "Whether OMEMO encryption is active")
   (last-sent-id :initform nil :accessor buffer-last-sent-id
                 :documentation "Stanza ID of last message we sent (for corrections)")
   (last-sent-text :initform nil :accessor buffer-last-sent-text
                   :documentation "Text of last message we sent (for corrections)")
   (correcting-p :initform nil :accessor buffer-correcting-p
                 :documentation "Whether we are currently editing a correction"))
  (:documentation "A chat buffer (DM, MUC, or system)"))

(defun make-buffer (&key name display-name (type :dm))
  (make-instance 'buffer :name name :display-name display-name :type type))

(defun buffer-message-duplicate-p (buffer message)
  "Check if a message with same text, nick, and timestamp already exists."
  (let ((msgs (buffer-messages buffer))
        (ts (message-timestamp message))
        (text (message-text message))
        (nick (message-nick message)))
    (loop for i from (max 0 (- (fill-pointer msgs) 50)) below (fill-pointer msgs)
          for m = (aref msgs i)
          thereis (and (= ts (message-timestamp m))
                       (equal text (message-text m))
                       (equal nick (message-nick m))))))

(defun buffer-add-message (buffer message)
  "Add a message to the buffer, inserting in timestamp order. Deduplicates."
  (when (buffer-message-duplicate-p buffer message)
    (return-from buffer-add-message message))
  (let* ((msgs (buffer-messages buffer))
         (ts (message-timestamp message))
         (count (fill-pointer msgs)))
    ;; If empty or newer than last message, just append
    (if (or (zerop count)
            (>= ts (message-timestamp (aref msgs (1- count)))))
        (vector-push-extend message msgs)
        ;; Otherwise insert in sorted position
        (let ((pos count))
          (vector-push-extend message msgs) ; extend first
          ;; Shift elements right to make room
          (loop while (and (> pos 0)
                           (< ts (message-timestamp (aref msgs (1- pos)))))
                do (setf (aref msgs pos) (aref msgs (1- pos)))
                   (decf pos))
          (setf (aref msgs pos) message))))
  (incf (buffer-unread-count buffer))
  (when (message-highlight-p message)
    (setf (buffer-mention-p buffer) t))
  message)

(defun buffer-correct-message (buffer replace-id new-text)
  "Find a message by stanza-id and replace its text. Returns T if found."
  (let ((msgs (buffer-messages buffer)))
    (loop for i from (max 0 (- (fill-pointer msgs) 100)) below (fill-pointer msgs)
          for m = (aref msgs i)
          when (and (message-stanza-id m) (string= (message-stanza-id m) replace-id))
          do (setf (message-text m) new-text
                   (message-edited-p m) t)
             (return t))))

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

;;; Participant helpers for MUC role tracking

(defun participant-nick (entry)
  "Get nick from a participant entry (nick . role)."
  (if (consp entry) (car entry) entry))

(defun participant-role (entry)
  "Get role from a participant entry (nick . role)."
  (if (consp entry) (cdr entry) :participant))

(defun role-prefix (role)
  "Return IRC-style prefix for a MUC role."
  (case role
    (:owner "~")
    (:admin "&")
    (:moderator "@")
    (:participant "+")
    (t "")))

(defun affiliation-to-role (affiliation role)
  "Map XMPP affiliation+role to a display role keyword.
   Affiliation: owner, admin, member, none, outcast.
   Role: moderator, participant, visitor, none."
  (cond
    ((equal affiliation "owner") :owner)
    ((equal affiliation "admin") :admin)
    ((equal role "moderator") :moderator)
    (t :participant)))

(defun buffer-add-participant (buffer nick &key (role :participant))
  "Add or update a participant in the buffer."
  (let ((existing (assoc nick (buffer-participants buffer) :test #'string=)))
    (if existing
        (setf (cdr existing) role)
        (push (cons nick role) (buffer-participants buffer)))))

(defun buffer-remove-participant (buffer nick)
  "Remove a participant from the buffer."
  (setf (buffer-participants buffer)
        (remove nick (buffer-participants buffer)
                :test #'string= :key #'car)))

(defun buffer-participant-nicks (buffer)
  "Get sorted list of participant nicks."
  (sort (mapcar #'car (buffer-participants buffer)) #'string<))

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
