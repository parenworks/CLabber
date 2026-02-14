;;;; clabber.asd - ASDF system definition for CLabber XMPP client

(asdf:defsystem #:clabber
  :description "CLabber - XMPP Chat Client with OMEMO"
  :author "Glenn Thompson"
  :license "MIT"
  :version "2.0.0"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:bordeaux-threads
               #:usocket
               #:cl+ssl
               #:ironclad
               #:babel
               #:cl-base64
               #:cffi)
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "packages")
     (:file "ansi")
     (:file "terminal")
     (:file "theme")
     (:file "model")
     (:file "config")
     (:file "widgets")
     (:file "layout")
     (:module "xmpp"
      :components
      ((:file "xml")
       (:file "stream")
       (:file "sasl")
       (:file "stanza")
       (:file "connection")))
     (:module "crypto"
      :components
      ((:file "signal-ffi")
       (:file "omemo")))
     (:file "app")
     (:file "main"))))
  :build-operation "program-op"
  :build-pathname "clabber"
  :entry-point "clabber:main")
