;;;; stanza.lisp - XMPP stanza classes
;;;;
;;;; CLOS classes for message, presence, and IQ stanzas.

(in-package #:clabber)

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
   (thread  :initarg :thread  :accessor message-thread  :initform nil))
  (:documentation "XMPP message stanza."))

(defmethod print-object ((m message-stanza) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "from=~a type=~a" (stanza-from m) (stanza-type m))))

;;; ============================================================
;;; Presence Stanza
;;; ============================================================

(defclass presence-stanza (stanza)
  ((show   :initarg :show   :accessor presence-show   :initform nil)
   (status :initarg :status :accessor presence-status :initform nil)
   (priority :initarg :priority :accessor presence-priority :initform nil))
  (:documentation "XMPP presence stanza."))

(defmethod print-object ((p presence-stanza) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "from=~a show=~a" (stanza-from p) (presence-show p))))

;;; ============================================================
;;; IQ Stanza
;;; ============================================================

(defclass iq-stanza (stanza)
  ((query :initarg :query :accessor iq-query :initform nil))
  (:documentation "XMPP IQ (info/query) stanza."))

(defmethod print-object ((iq iq-stanza) stream)
  (print-unreadable-object (iq stream :type t)
    (format stream "id=~a type=~a" (stanza-id iq) (stanza-type iq))))

;;; ============================================================
;;; Stanza Parsing
;;; ============================================================

(defgeneric parse-stanza (xml-element)
  (:documentation "Parse an XML element into a stanza object."))

(defmethod parse-stanza ((el xml-element))
  "Parse an xml-element into the appropriate stanza type."
  (let ((name (xml-name el)))
    (cond
      ((string= name "message") (parse-message-stanza el))
      ((string= name "presence") (parse-presence-stanza el))
      ((string= name "iq") (parse-iq-stanza el))
      (t el))))  ; Return raw element for unknown types

(defun parse-message-stanza (el)
  "Parse a message stanza from XML."
  (let ((body-el (xml-child el "body"))
        (subject-el (xml-child el "subject"))
        (thread-el (xml-child el "thread")))
    (make-instance 'message-stanza
                   :id (xml-attr el "id")
                   :to (xml-attr el "to")
                   :from (xml-attr el "from")
                   :type (xml-attr el "type")
                   :body (when body-el (xml-text body-el))
                   :subject (when subject-el (xml-text subject-el))
                   :thread (when thread-el (xml-text thread-el))
                   :xml el)))

(defun parse-presence-stanza (el)
  "Parse a presence stanza from XML."
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
  "Parse an IQ stanza from XML."
  (make-instance 'iq-stanza
                 :id (xml-attr el "id")
                 :to (xml-attr el "to")
                 :from (xml-attr el "from")
                 :type (xml-attr el "type")
                 :query (first (xml-children el))
                 :xml el))

;;; ============================================================
;;; Stanza Serialization
;;; ============================================================

(defgeneric serialize-stanza (stanza)
  (:documentation "Serialize a stanza to XML string."))

(defmethod serialize-stanza ((m message-stanza))
  "Serialize a message stanza to XML."
  (let ((el (make-xml-element "message"
                              :attributes `(,@(when (stanza-to m) 
                                                `(("to" . ,(stanza-to m))))
                                            ,@(when (stanza-type m) 
                                                `(("type" . ,(stanza-type m))))
                                            ,@(when (stanza-id m) 
                                                `(("id" . ,(stanza-id m)))))
                              :children `(,@(when (message-body m)
                                              (list (make-xml-element "body" 
                                                                      :text (message-body m))))
                                          ,@(when (message-subject m)
                                              (list (make-xml-element "subject" 
                                                                      :text (message-subject m))))))))
    (serialize-xml el)))

(defmethod serialize-stanza ((p presence-stanza))
  "Serialize a presence stanza to XML."
  (let ((el (make-xml-element "presence"
                              :attributes `(,@(when (stanza-to p) 
                                                `(("to" . ,(stanza-to p))))
                                            ,@(when (stanza-type p) 
                                                `(("type" . ,(stanza-type p)))))
                              :children `(,@(when (presence-show p)
                                              (list (make-xml-element "show" 
                                                                      :text (presence-show p))))
                                          ,@(when (presence-status p)
                                              (list (make-xml-element "status" 
                                                                      :text (presence-status p))))))))
    (serialize-xml el)))

(defmethod serialize-stanza ((iq iq-stanza))
  "Serialize an IQ stanza to XML."
  (let ((el (make-xml-element "iq"
                              :attributes `(("type" . ,(or (stanza-type iq) "get"))
                                            ("id" . ,(or (stanza-id iq) (generate-id)))
                                            ,@(when (stanza-to iq) 
                                                `(("to" . ,(stanza-to iq)))))
                              :children (when (iq-query iq) 
                                          (list (iq-query iq))))))
    (serialize-xml el)))

;;; ============================================================
;;; Helpers
;;; ============================================================

(defvar *stanza-id-counter* 0)

(defun generate-id ()
  "Generate a unique stanza ID."
  (format nil "clabber-~d" (incf *stanza-id-counter*)))

(defun make-message-stanza (to body &key type id)
  "Create a new message stanza."
  (make-instance 'message-stanza
                 :to to
                 :body body
                 :type type
                 :id (or id (generate-id))))

(defun make-presence-stanza (&key to type show status)
  "Create a new presence stanza."
  (make-instance 'presence-stanza
                 :to to
                 :type type
                 :show show
                 :status status))

(defun make-iq-stanza (type &key id to query)
  "Create a new IQ stanza."
  (make-instance 'iq-stanza
                 :type type
                 :id (or id (generate-id))
                 :to to
                 :query query))
