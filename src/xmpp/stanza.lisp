(in-package #:clabber.xmpp)

;;; XMPP Stanza Classes and Parsing

;;; ============================================================
;;; Base Stanza Class
;;; ============================================================

(defclass stanza ()
  ((id   :initarg :id   :accessor stanza-id   :initform nil)
   (to   :initarg :to   :accessor stanza-to   :initform nil)
   (from :initarg :from :accessor stanza-from :initform nil)
   (type :initarg :type :accessor stanza-type :initform nil)
   (xml  :initarg :xml  :accessor stanza-xml  :initform nil))
  (:documentation "Base class for XMPP stanzas."))

;;; ============================================================
;;; Message Stanza
;;; ============================================================

(defclass message-stanza (stanza)
  ((body    :initarg :body    :accessor message-body    :initform nil)
   (subject :initarg :subject :accessor message-subject :initform nil)
   (thread  :initarg :thread  :accessor message-thread  :initform nil)
   (delay   :initarg :delay   :accessor message-delay   :initform nil
            :documentation "XEP-0203 delay timestamp")
   (chat-state :initarg :chat-state :accessor message-chat-state :initform nil
               :documentation "XEP-0085 chat state")
   (omemo-encrypted :initarg :omemo-encrypted :accessor message-omemo-encrypted :initform nil
                    :documentation "XEP-0384 OMEMO encrypted element")))

;;; ============================================================
;;; Presence Stanza
;;; ============================================================

(defclass presence-stanza (stanza)
  ((show   :initarg :show   :accessor presence-show   :initform nil)
   (status :initarg :status :accessor presence-status :initform nil)
   (priority :initarg :priority :accessor presence-priority :initform nil)))

;;; ============================================================
;;; IQ Stanza
;;; ============================================================

(defclass iq-stanza (stanza)
  ((query :initarg :query :accessor iq-query :initform nil)))

;;; ============================================================
;;; ID Generation
;;; ============================================================

(defvar *stanza-id-counter* 0)

(defun generate-id ()
  (format nil "clabber-~d" (incf *stanza-id-counter*)))

;;; ============================================================
;;; Stanza Constructors
;;; ============================================================

(defun make-message-stanza (to body &key type id)
  (make-instance 'message-stanza
                 :to to :body body :type type
                 :id (or id (generate-id))))

(defun make-presence-stanza (&key to type show status)
  (make-instance 'presence-stanza
                 :to to :type type :show show :status status))

(defun make-iq-stanza (type &key id to query)
  (make-instance 'iq-stanza
                 :type type :id (or id (generate-id))
                 :to to :query query))

;;; ============================================================
;;; Stanza Parsing
;;; ============================================================

(defun find-omemo-encrypted (el)
  "Find OMEMO encrypted element in message."
  (or (xml-child-by-ns el "urn:xmpp:omemo:2")
      (xml-child-by-ns el "eu.siacs.conversations.axolotl")
      (xml-child el "encrypted")))

(defun parse-stanza (el)
  "Parse an xml-element into the appropriate stanza type."
  (let ((name (xml-name el)))
    (cond
      ((string= name "message") (parse-message-stanza el))
      ((string= name "presence") (parse-presence-stanza el))
      ((string= name "iq") (parse-iq-stanza el))
      (t el))))

(defun parse-message-stanza (el)
  (let ((result-el (xml-child el "result")))
    (if result-el
        ;; MAM result - extract forwarded message
        (let* ((forwarded-el (xml-child result-el "forwarded"))
               (delay-el (when forwarded-el (xml-child forwarded-el "delay")))
               (inner-msg (when forwarded-el (xml-child forwarded-el "message")))
               (body-el (when inner-msg (xml-child inner-msg "body")))
               (omemo-el (when inner-msg (find-omemo-encrypted inner-msg))))
          (when inner-msg
            (make-instance 'message-stanza
                           :id (or (xml-attr result-el "id") (xml-attr el "id"))
                           :to (xml-attr inner-msg "to")
                           :from (xml-attr inner-msg "from")
                           :type (or (xml-attr inner-msg "type") "groupchat")
                           :body (when body-el (decode-xml-entities (xml-text body-el)))
                           :delay (when delay-el (xml-attr delay-el "stamp"))
                           :omemo-encrypted omemo-el
                           :xml el)))
        ;; Regular message
        (let ((body-el (xml-child el "body"))
              (subject-el (xml-child el "subject"))
              (thread-el (xml-child el "thread"))
              (delay-el (xml-child el "delay"))
              (omemo-el (find-omemo-encrypted el))
              (chat-state (or (when (xml-child el "composing") "composing")
                              (when (xml-child el "active") "active")
                              (when (xml-child el "paused") "paused")
                              (when (xml-child el "inactive") "inactive")
                              (when (xml-child el "gone") "gone"))))
          (make-instance 'message-stanza
                         :id (xml-attr el "id")
                         :to (xml-attr el "to")
                         :from (xml-attr el "from")
                         :type (xml-attr el "type")
                         :body (when body-el (decode-xml-entities (xml-text body-el)))
                         :subject (when subject-el (decode-xml-entities (xml-text subject-el)))
                         :thread (when thread-el (xml-text thread-el))
                         :delay (when delay-el (xml-attr delay-el "stamp"))
                         :chat-state chat-state
                         :omemo-encrypted omemo-el
                         :xml el)))))

(defun parse-presence-stanza (el)
  (let ((show-el (xml-child el "show"))
        (status-el (xml-child el "status"))
        (priority-el (xml-child el "priority")))
    (make-instance 'presence-stanza
                   :id (xml-attr el "id")
                   :to (xml-attr el "to")
                   :from (xml-attr el "from")
                   :type (xml-attr el "type")
                   :show (when show-el (xml-text show-el))
                   :status (when status-el (xml-text status-el))
                   :priority (when priority-el
                               (parse-integer (xml-text priority-el) :junk-allowed t))
                   :xml el)))

(defun parse-iq-stanza (el)
  (make-instance 'iq-stanza
                 :id (xml-attr el "id")
                 :to (xml-attr el "to")
                 :from (xml-attr el "from")
                 :type (xml-attr el "type")
                 :query (first (xml-children el))
                 :xml el))

;;; ============================================================
;;; Stanza to XML Conversion
;;; ============================================================

(defun stanza-to-xml (stanza)
  "Convert a stanza to an xml-element for sending."
  (typecase stanza
    (message-stanza
     (make-xml-element "message"
                       :attributes `(,@(when (stanza-to stanza) `(("to" . ,(stanza-to stanza))))
                                     ,@(when (stanza-type stanza) `(("type" . ,(stanza-type stanza))))
                                     ,@(when (stanza-id stanza) `(("id" . ,(stanza-id stanza)))))
                       :children `(,@(when (message-body stanza)
                                       (list (make-xml-element "body" :text (message-body stanza)))))))
    (presence-stanza
     (make-xml-element "presence"
                       :attributes `(,@(when (stanza-to stanza) `(("to" . ,(stanza-to stanza))))
                                     ,@(when (stanza-type stanza) `(("type" . ,(stanza-type stanza)))))
                       :children `(,@(when (presence-show stanza)
                                       (list (make-xml-element "show" :text (presence-show stanza))))
                                   ,@(when (presence-status stanza)
                                       (list (make-xml-element "status" :text (presence-status stanza)))))))
    (iq-stanza
     (make-xml-element "iq"
                       :attributes `(("type" . ,(or (stanza-type stanza) "get"))
                                     ("id" . ,(or (stanza-id stanza) (generate-id)))
                                     ,@(when (stanza-to stanza) `(("to" . ,(stanza-to stanza)))))
                       :children (when (iq-query stanza) (list (iq-query stanza)))))
    (t stanza)))

;;; ============================================================
;;; JID Utilities
;;; ============================================================

(defun parse-jid (jid)
  "Parse JID into (values user domain resource)."
  (let* ((at-pos (position #\@ jid))
         (slash-pos (position #\/ jid)))
    (values (if at-pos (subseq jid 0 at-pos) jid)
            (if at-pos (subseq jid (1+ at-pos) (or slash-pos (length jid))) nil)
            (if slash-pos (subseq jid (1+ slash-pos)) nil))))

(defun bare-jid (full-jid)
  "Extract bare JID (user@domain) from full JID."
  (let ((slash-pos (position #\/ full-jid)))
    (if slash-pos (subseq full-jid 0 slash-pos) full-jid)))

(defun jid-resource (full-jid)
  "Extract resource from full JID."
  (let ((slash-pos (position #\/ full-jid)))
    (when slash-pos (subseq full-jid (1+ slash-pos)))))
