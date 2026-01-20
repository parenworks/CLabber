;;;; events.lisp - Event classes and apply-event methods for CLabber

(in-package #:clabber)

(defclass event () ())

(defclass xmpp-connecting (event)
  ((jid :initarg :jid :reader evt-jid)))

(defclass xmpp-connected (event)
  ((jid :initarg :jid :reader evt-jid)))

(defclass xmpp-disconnected (event) ())

(defclass xmpp-message (event)
  ((from :initarg :from :reader evt-from)
   (body :initarg :body :reader evt-body)
   (timestamp :initarg :timestamp :initform nil :reader evt-timestamp
              :documentation "XEP-0203 delay timestamp or current time")
   (msg-id :initarg :msg-id :initform nil :reader evt-msg-id
           :documentation "Message ID for deduplication")
   (msg-type :initarg :msg-type :initform nil :reader evt-msg-type
             :documentation "Message type: chat, groupchat, etc.")))

(defclass xmpp-presence (event)
  ((jid :initarg :jid :reader evt-jid)
   (show :initarg :show :reader evt-show)
   (status :initarg :status :initform nil :reader evt-status)))

(defclass chat-state-event (event)
  ((from :initarg :from :reader evt-from)
   (state :initarg :state :reader evt-state
          :documentation "Chat state: active, composing, paused, inactive, gone"))
  (:documentation "XEP-0085 chat state notification."))

(defclass muc-presence (event)
  ((room :initarg :room :reader evt-room
         :documentation "Bare JID of the MUC room")
   (nick :initarg :nick :reader evt-nick
         :documentation "Nickname of the participant")
   (show :initarg :show :reader evt-show
         :documentation "Presence status: available, away, dnd, xa, offline")
   (affiliation :initarg :affiliation :initform nil :reader evt-affiliation)
   (role :initarg :role :initform nil :reader evt-role))
  (:documentation "MUC room participant presence update."))

(defclass roster-update (event)
  ((items :initarg :items :reader evt-items)))

(defclass bookmarks-update (event)
  ((rooms :initarg :rooms :reader evt-rooms))
  (:documentation "MUC bookmarks received from server."))

(defclass error-event (event)
  ((where :initarg :where :reader evt-where)
   (condition :initarg :condition :reader evt-condition)))

(defgeneric apply-event (evt st ly)
  (:documentation "Apply event to state. UI thread only; layout used for visibility/unread."))

(defmethod apply-event ((e xmpp-connecting) (st app-state) (ly layout))
  (declare (ignore ly))
  (setf (state-status st) "connecting")
  (state-log st (format nil "Connecting as ~a..." (evt-jid e)))
  st)

(defmethod apply-event ((e xmpp-connected) (st app-state) (ly layout))
  (declare (ignore ly))
  (setf (state-connected-p st) t
        (state-status st) "online"
        (state-my-jid st) (evt-jid e)
        (state-my-nick st) (let ((jid (evt-jid e)))
                            (subseq jid 0 (position #\@ jid))))
  (state-log st "Connected.")
  st)

(defmethod apply-event ((e xmpp-disconnected) (st app-state) (ly layout))
  (declare (ignore e ly))
  (setf (state-connected-p st) nil
        (state-status st) "offline")
  (state-log st "Disconnected.")
  st)

(defun bare-jid (jid)
  "Extract bare JID (user@domain) from full JID (user@domain/resource)."
  (let ((slash-pos (position #\/ jid)))
    (if slash-pos
        (subseq jid 0 slash-pos)
        jid)))

(defun jid-resource (jid)
  "Extract resource (nickname) from full JID."
  (let ((slash-pos (position #\/ jid)))
    (if slash-pos
        (subseq jid (1+ slash-pos))
        nil)))

(defun format-timestamp (iso-timestamp)
  "Format ISO 8601 timestamp to HH:MM for display."
  (when (and iso-timestamp (>= (length iso-timestamp) 16))
    ;; ISO format: 2026-01-20T08:30:00Z
    ;; Extract HH:MM from positions 11-16
    (let ((time-part (subseq iso-timestamp 11 16)))
      (format nil "[~a]" time-part))))

(defmethod apply-event ((e xmpp-message) (st app-state) (ly layout))
  (let* ((full-from (evt-from e))
         (msg-type (evt-msg-type e))
         ;; For MUC messages, use bare JID as buffer key, resource as sender name
         (bare-from (bare-jid full-from))
         (sender (or (jid-resource full-from) bare-from))
         ;; Check if this is a MUC room
         (roster-item (find bare-from (state-roster st) :key #'roster-jid :test #'string=))
         (is-muc (and roster-item (string= (roster-presence roster-item) "muc")))
         ;; MUC private message: from room/nick but type="chat" (not "groupchat")
         (is-muc-private (and is-muc 
                              (jid-resource full-from)
                              (or (null msg-type) 
                                  (string= msg-type "chat"))))
         ;; Skip own MUC messages (server echo) - we already show "me:" locally
         (my-nick (state-my-nick st))
         (is-own-muc-echo (and is-muc 
                               (not is-muc-private)
                               my-nick 
                               (string= sender my-nick)))
         ;; Get message ID for deduplication
         (msg-id (evt-msg-id e))
         ;; Determine buffer key:
         ;; - MUC private: use full JID (room/nick) as buffer
         ;; - MUC groupchat: use bare JID (room) as buffer
         ;; - Regular DM: use full-from
         (from (cond
                 (is-muc-private full-from)  ; Private chat with room/nick
                 (is-muc bare-from)          ; Room groupchat
                 (t full-from)))             ; Regular DM
         (buf (state-ensure-buffer st from :title from))
         ;; Check if we've already seen this message
         (already-seen (and msg-id 
                            (gethash msg-id (buffer-seen-ids buf)))))
    ;; Don't display duplicates or our own MUC echo
    (unless (or is-own-muc-echo already-seen)
      ;; Mark message as seen
      (when msg-id
        (setf (gethash msg-id (buffer-seen-ids buf)) t))
      (let* ((display-name (cond
                             (is-muc-private sender)  ; Show nick for private
                             (is-muc sender)          ; Show nick for groupchat
                             (t full-from)))          ; Show full JID for DM
             (timestamp (format-timestamp (evt-timestamp e))))
        (buffer-add-line buf (format nil "~@[~a ~]~a: ~a" timestamp display-name (evt-body e)))
        (cond
          ((buffer-visible-p ly from)
           (setf (buffer-unread buf) 0)
           (when (buffer-open-p st from)
             (touch-open-buffer! st from)))
          ((buffer-open-p st from)
           (incf (buffer-unread buf))
           (touch-open-buffer! st from))
          (t
           (incf (buffer-unread buf))
           (state-log st (format nil "New message from ~a (unopened)." from))
           (when (state-auto-open-on-message-p st)
             (open-buffer! st from)
             (touch-open-buffer! st from)))))))
  st)

(defmethod apply-event ((e xmpp-presence) (st app-state) (ly layout))
  (declare (ignore ly))
  (let* ((jid (evt-jid e))
         (item (find jid (state-roster st) :key #'roster-jid :test #'string=)))
    (when item
      (setf (roster-presence item) (or (evt-show e) "available"))))
  st)

(defmethod apply-event ((e muc-presence) (st app-state) (ly layout))
  (declare (ignore ly))
  (let* ((room (evt-room e))
         (nick (evt-nick e))
         (show (evt-show e))
         ;; Ensure buffer exists for MUC room
         (buf (ensure-buffer st room :title room)))
    (let ((participants (buffer-participants buf)))
      (if (string= show "offline")
          (remhash nick participants)
          (setf (gethash nick participants) show))))
  st)

(defmethod apply-event ((e chat-state-event) (st app-state) (ly layout))
  (declare (ignore ly))
  (let* ((from (evt-from e))
         (state (evt-state e))
         (bare-from (bare-jid from))
         (buf (find-buffer st bare-from)))
    (when buf
      (let ((typing (buffer-typing-users buf)))
        (if (string= state "composing")
            ;; User is typing - add to typing list with timestamp
            (setf (gethash from typing) (get-universal-time))
            ;; User stopped typing - remove from list
            (remhash from typing)))))
  st)

(defmethod apply-event ((e roster-update) (st app-state) (ly layout))
  (declare (ignore ly))
  ;; Keep existing MUC rooms (presence = "muc") and replace regular contacts
  (let ((mucs (remove-if-not (lambda (item) (string= (roster-presence item) "muc"))
                              (state-roster st))))
    (setf (state-roster st) (append (evt-items e) mucs)))
  (state-log st (format nil "Roster updated: ~d contacts." (length (evt-items e))))
  st)

(defmethod apply-event ((e error-event) (st app-state) (ly layout))
  (declare (ignore ly))
  (state-log st (format nil "Error (~a): ~a" (evt-where e) (evt-condition e)))
  st)

(defmethod apply-event ((e bookmarks-update) (st app-state) (ly layout))
  (declare (ignore ly))
  (let ((rooms (evt-rooms e)))
    (state-log st (format nil "Bookmarks: ~d MUC rooms." (length rooms)))
    ;; Add MUC rooms to roster as special items
    (dolist (room rooms)
      (let ((jid (getf room :jid))
            (name (getf room :name)))
        (push (make-instance 'roster-item
                             :jid jid
                             :name (or name jid)
                             :presence "muc")
              (state-roster st)))))
  st)

(defmethod apply-event ((e event) (st app-state) (ly layout))
  (declare (ignore e ly))
  st)
