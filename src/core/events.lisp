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
   (body :initarg :body :reader evt-body)))

(defclass xmpp-presence (event)
  ((jid :initarg :jid :reader evt-jid)
   (show :initarg :show :reader evt-show)
   (status :initarg :status :initform nil :reader evt-status)))

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

(defmethod apply-event ((e xmpp-message) (st app-state) (ly layout))
  (let* ((full-from (evt-from e))
         ;; For MUC messages, use bare JID as buffer key, resource as sender name
         (bare-from (bare-jid full-from))
         (sender (or (jid-resource full-from) bare-from))
         ;; Check if this is a MUC room
         (roster-item (find bare-from (state-roster st) :key #'roster-jid :test #'string=))
         (is-muc (and roster-item (string= (roster-presence roster-item) "muc")))
         ;; Skip own MUC messages (server echo) - we already show "me:" locally
         (my-nick (state-my-nick st))
         (is-own-muc-echo (and is-muc my-nick (string= sender my-nick))))
    ;; Don't display our own MUC messages (already shown as "me:")
    (unless is-own-muc-echo
      (let* ((from (if is-muc bare-from full-from))
             (display-name (if is-muc sender full-from))
             (buf (state-ensure-buffer st from :title from)))
        (buffer-add-line buf (format nil "~a: ~a" display-name (evt-body e)))
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
