;;;; xml.lisp - XML parsing and serialization for XMPP
;;;;
;;;; Uses klacks (streaming SAX-like parser from cxml) for parsing
;;;; incoming XML streams, and simple string building for outgoing.

(in-package #:clabber)

;;; ============================================================
;;; XML Namespaces
;;; ============================================================

(defparameter +ns-stream+ "http://etherx.jabber.org/streams")
(defparameter +ns-client+ "jabber:client")
(defparameter +ns-sasl+ "urn:ietf:params:xml:ns:xmpp-sasl")
(defparameter +ns-tls+ "urn:ietf:params:xml:ns:xmpp-tls")
(defparameter +ns-bind+ "urn:ietf:params:xml:ns:xmpp-bind")
(defparameter +ns-session+ "urn:ietf:params:xml:ns:xmpp-session")
(defparameter +ns-roster+ "jabber:iq:roster")
(defparameter +ns-disco-info+ "http://jabber.org/protocol/disco#info")
(defparameter +ns-disco-items+ "http://jabber.org/protocol/disco#items")
(defparameter +ns-muc+ "http://jabber.org/protocol/muc")
(defparameter +ns-muc-user+ "http://jabber.org/protocol/muc#user")
(defparameter +ns-pubsub+ "http://jabber.org/protocol/pubsub")
(defparameter +ns-bookmarks+ "storage:bookmarks")
(defparameter +ns-bookmarks2+ "urn:xmpp:bookmarks:1")
(defparameter +ns-private+ "jabber:iq:private")

;;; ============================================================
;;; XML Element Class
;;; ============================================================

(defclass xml-element ()
  ((name       :initarg :name       :accessor xml-name)
   (namespace  :initarg :namespace  :accessor xml-namespace :initform nil)
   (attributes :initarg :attributes :accessor xml-attributes :initform nil)
   (children   :initarg :children   :accessor xml-children :initform nil)
   (text       :initarg :text       :accessor xml-text :initform nil))
  (:documentation "Represents an XML element with attributes and children."))

(defmethod print-object ((el xml-element) stream)
  (print-unreadable-object (el stream :type t)
    (format stream "~a" (xml-name el))))

(defun make-xml-element (name &key namespace attributes children text)
  "Create a new XML element."
  (make-instance 'xml-element
                 :name name
                 :namespace namespace
                 :attributes attributes
                 :children children
                 :text text))

(defun xml-attr (element attr-name)
  "Get an attribute value from an XML element."
  (cdr (assoc attr-name (xml-attributes element) :test #'string=)))

(defun xml-child (element child-name)
  "Get the first child element with the given name."
  (find child-name (xml-children element) 
        :key #'xml-name :test #'string=))

(defun xml-children-named (element child-name)
  "Get all child elements with the given name."
  (remove-if-not (lambda (c) (string= (xml-name c) child-name))
                 (xml-children element)))

(defun xml-child-by-ns (element namespace)
  "Get the first child element with the given namespace."
  (find-if (lambda (c)
             (and (typep c 'xml-element)
                  (let ((ns (xml-namespace c)))
                    (and ns (string= ns namespace)))))
           (xml-children element)))

;;; ============================================================
;;; XML Serialization (Output)
;;; ============================================================

(defun xml-escape (string)
  "Escape special XML characters in a string."
  (with-output-to-string (out)
    (loop for char across string do
      (case char
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\& (write-string "&amp;" out))
        (#\' (write-string "&apos;" out))
        (#\" (write-string "&quot;" out))
        (otherwise (write-char char out))))))

(defun serialize-xml (element &optional (stream nil))
  "Serialize an XML element to a string or stream."
  (let ((out (or stream (make-string-output-stream))))
    (format out "<~a" (xml-name element))
    ;; Namespace
    (when (xml-namespace element)
      (format out " xmlns='~a'" (xml-namespace element)))
    ;; Attributes
    (loop for (name . value) in (xml-attributes element) do
      (format out " ~a='~a'" name (xml-escape value)))
    ;; Children or self-close
    (cond
      ((or (xml-children element) (xml-text element))
       (write-string ">" out)
       (when (xml-text element)
         (write-string (xml-escape (xml-text element)) out))
       (dolist (child (xml-children element))
         (serialize-xml child out))
       (format out "</~a>" (xml-name element)))
      (t
       (write-string "/>" out)))
    (unless stream
      (get-output-stream-string out))))

;;; ============================================================
;;; Stream Opening/Closing
;;; ============================================================

(defun make-stream-open (to)
  "Create the opening stream tag for XMPP."
  (format nil "<?xml version='1.0'?><stream:stream to='~a' xmlns='~a' xmlns:stream='~a' version='1.0'>"
          to +ns-client+ +ns-stream+))

(defun make-stream-close ()
  "Create the closing stream tag."
  "</stream:stream>")

;;; ============================================================
;;; Stanza Building Helpers
;;; ============================================================

(defun make-iq (type id &key to from children)
  "Build an IQ stanza element."
  (make-xml-element "iq"
                    :attributes `(("type" . ,type)
                                  ("id" . ,id)
                                  ,@(when to `(("to" . ,to)))
                                  ,@(when from `(("from" . ,from))))
                    :children children))

(defun make-message (to body &key type id from)
  "Build a message stanza element."
  (make-xml-element "message"
                    :attributes `(("to" . ,to)
                                  ,@(when type `(("type" . ,type)))
                                  ,@(when id `(("id" . ,id)))
                                  ,@(when from `(("from" . ,from))))
                    :children (list (make-xml-element "body" :text body))))

(defun make-presence (&key to type show status children)
  "Build a presence stanza element."
  (make-xml-element "presence"
                    :attributes `(,@(when to `(("to" . ,to)))
                                  ,@(when type `(("type" . ,type))))
                    :children `(,@(when show 
                                    (list (make-xml-element "show" :text show)))
                                ,@(when status 
                                    (list (make-xml-element "status" :text status)))
                                ,@children)))
