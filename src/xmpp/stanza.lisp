(in-package #:clabber.xmpp)

;;; XMPP Stanza Types
;;; TODO: Port XML stanza parsing from feature/native-xmpp branch

(defclass stanza ()
  ((id :initarg :id :accessor stanza-id :initform nil)
   (from :initarg :from :accessor stanza-from :initform nil)
   (to :initarg :to :accessor stanza-to :initform nil)
   (type :initarg :type :accessor stanza-type :initform nil))
  (:documentation "Base XMPP stanza"))

(defclass message-stanza (stanza)
  ((body :initarg :body :accessor stanza-body :initform nil)
   (thread :initarg :thread :accessor stanza-thread :initform nil))
  (:documentation "XMPP message stanza"))

(defclass presence-stanza (stanza)
  ((show :initarg :show :accessor stanza-show :initform nil)
   (status :initarg :status :accessor stanza-status :initform nil))
  (:documentation "XMPP presence stanza"))

(defclass iq-stanza (stanza)
  ((query :initarg :query :accessor stanza-query :initform nil))
  (:documentation "XMPP IQ stanza"))
