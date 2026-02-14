(in-package #:clabber.xmpp)

;;; XML Element class, parsing, and serialization for XMPP
;;; Uses simple string-based parsing (no cxml dependency)

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
(defparameter +ns-carbons+ "urn:xmpp:carbons:2")
(defparameter +ns-chatstates+ "http://jabber.org/protocol/chatstates")
(defparameter +ns-mam+ "urn:xmpp:mam:2")
(defparameter +ns-forward+ "urn:xmpp:forward:0")

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
  (make-instance 'xml-element
                 :name name :namespace namespace
                 :attributes attributes :children children :text text))

(defun xml-attr (element attr-name)
  "Get an attribute value from an XML element."
  (cdr (assoc attr-name (xml-attributes element) :test #'string=)))

(defun xml-child (element child-name)
  "Get the first child element with the given name."
  (find child-name (xml-children element)
        :key (lambda (c) (when (typep c 'xml-element) (xml-name c)))
        :test #'string=))

(defun xml-children-named (element child-name)
  "Get all child elements with the given name."
  (remove-if-not (lambda (c)
                   (and (typep c 'xml-element)
                        (string= (xml-name c) child-name)))
                 (xml-children element)))

(defun xml-child-by-ns (element namespace)
  "Get the first child element with the given namespace."
  (find-if (lambda (c)
             (and (typep c 'xml-element)
                  (let ((ns (xml-namespace c)))
                    (and ns (string= ns namespace)))))
           (xml-children element)))

;;; ============================================================
;;; XML Serialization
;;; ============================================================

(defun xml-escape (string)
  "Escape special XML characters."
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
    (when (xml-namespace element)
      (format out " xmlns='~a'" (xml-namespace element)))
    (loop for (name . value) in (xml-attributes element) do
      (format out " ~a='~a'" name (xml-escape value)))
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
  (format nil "<?xml version='1.0'?><stream:stream to='~a' xmlns='~a' xmlns:stream='~a' version='1.0'>"
          to +ns-client+ +ns-stream+))

(defun make-stream-close ()
  "</stream:stream>")

;;; ============================================================
;;; Stanza Building Helpers
;;; ============================================================

(defun make-iq (type id &key to from children)
  (make-xml-element "iq"
                    :attributes `(("type" . ,type)
                                  ("id" . ,id)
                                  ,@(when to `(("to" . ,to)))
                                  ,@(when from `(("from" . ,from))))
                    :children children))

(defun make-presence-xml (&key to type show status children)
  (make-xml-element "presence"
                    :attributes `(,@(when to `(("to" . ,to)))
                                  ,@(when type `(("type" . ,type))))
                    :children `(,@(when show
                                    (list (make-xml-element "show" :text show)))
                                ,@(when status
                                    (list (make-xml-element "status" :text status)))
                                ,@children)))

;;; ============================================================
;;; XML Parsing
;;; ============================================================

(defun whitespace-char-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return)))

(defun parse-attributes (attr-string)
  "Parse attributes from a string like \" foo='bar' baz='qux'\"."
  (let ((attrs nil)
        (pos 0)
        (len (length attr-string)))
    (loop while (< pos len)
          for eq-pos = (position #\= attr-string :start pos)
          while eq-pos
          do (let* ((name-start (position-if-not #'whitespace-char-p attr-string :start pos))
                    (name (when (and name-start (< name-start eq-pos))
                            (subseq attr-string name-start eq-pos)))
                    (quote-char (when (< (1+ eq-pos) len) (char attr-string (1+ eq-pos))))
                    (val-start (+ eq-pos 2))
                    (val-end (when (and quote-char (< val-start len))
                               (position quote-char attr-string :start val-start))))
               (when (and name val-end)
                 (push (cons name (subseq attr-string val-start val-end)) attrs))
               (setf pos (if val-end (1+ val-end) len))))
    (nreverse attrs)))

(defun decode-xml-entities (text)
  "Decode XML entities in TEXT."
  (when text
    (let ((result text))
      (setf result (cl-ppcre:regex-replace-all "&amp;" result "&"))
      (setf result (cl-ppcre:regex-replace-all "&lt;" result "<"))
      (setf result (cl-ppcre:regex-replace-all "&gt;" result ">"))
      (setf result (cl-ppcre:regex-replace-all "&quot;" result "\""))
      (setf result (cl-ppcre:regex-replace-all "&apos;" result "'"))
      result)))

(defun parse-full-element (xml-string)
  "Parse a complete XML element with children from string."
  (when (and xml-string (> (length xml-string) 2))
    (let* ((start (position #\< xml-string))
           (first-gt (position #\> xml-string)))
      (when (and start first-gt)
        (let* ((tag-content (subseq xml-string (1+ start) first-gt))
               (space-pos (position #\Space tag-content))
               (slash-pos (position #\/ tag-content))
               (name (cond
                       (space-pos (subseq tag-content 0 space-pos))
                       (slash-pos (subseq tag-content 0 slash-pos))
                       (t tag-content)))
               (attrs (when space-pos
                        (parse-attributes (subseq tag-content space-pos))))
               (xmlns (cdr (assoc "xmlns" attrs :test #'string=)))
               (end-tag-start (search (format nil "</~a>" name) xml-string))
               (text (when (and end-tag-start (> end-tag-start (1+ first-gt)))
                       (subseq xml-string (1+ first-gt) end-tag-start)))
               (children (parse-child-elements xml-string (1+ first-gt) end-tag-start)))
          (make-xml-element name
                            :namespace xmlns
                            :attributes (remove "xmlns" attrs :key #'car :test #'string=)
                            :text (when (and text (not children)) text)
                            :children children))))))

(defun parse-child-elements (xml-string start end)
  "Parse child elements from XML string between start and end positions."
  (when (and start end (< start end))
    (let ((children nil)
          (pos start))
      (loop while (< pos end)
            for tag-start = (position #\< xml-string :start pos)
            while (and tag-start (< tag-start end))
            do (let ((tag-end (position #\> xml-string :start tag-start)))
                 (when tag-end
                   (let* ((tag-content (subseq xml-string (1+ tag-start) tag-end))
                          (is-closing (and (> (length tag-content) 0)
                                           (char= (char tag-content 0) #\/)))
                          (is-self-closing (and (> (length tag-content) 0)
                                                (char= (char tag-content (1- (length tag-content))) #\/))))
                     (cond
                       (is-closing
                        (setf pos (1+ tag-end)))
                       (is-self-closing
                        (let* ((space-pos (position #\Space tag-content))
                               (name (subseq tag-content 0 (or space-pos (1- (length tag-content)))))
                               (attrs (when space-pos (parse-attributes (subseq tag-content space-pos))))
                               (xmlns (cdr (assoc "xmlns" attrs :test #'string=))))
                          (push (make-xml-element name
                                                  :namespace xmlns
                                                  :attributes (remove "xmlns" attrs :key #'car :test #'string=))
                                children))
                        (setf pos (1+ tag-end)))
                       (t
                        (let* ((space-pos (position #\Space tag-content))
                               (name (subseq tag-content 0 (or space-pos (length tag-content))))
                               (close-tag (format nil "</~a>" name))
                               (close-pos (search close-tag xml-string :start2 tag-end)))
                          (when close-pos
                            (let ((child-xml (subseq xml-string tag-start (+ close-pos (length close-tag)))))
                              (push (parse-full-element child-xml) children)))
                          (setf pos (if close-pos (+ close-pos (length close-tag)) (1+ tag-end))))))))))
      (nreverse children))))
