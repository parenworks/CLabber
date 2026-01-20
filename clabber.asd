;;;; clabber.asd - ASDF system definition for CLabber XMPP client

(asdf:defsystem #:clabber
  :description "A Common Lisp TUI XMPP (Jabber) client"
  :author "Glenn Thompson"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-xmpp
               #:cl-xmpp-tls
               #:cl-xmpp-sasl
               #:croatoan
               #:bordeaux-threads
               #:alexandria
               #:local-time
               #:str)
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "config")
     (:module "core"
      :components
      ((:file "classes")
       (:file "queue")
       (:file "events")
       (:file "commands")
       (:file "dispatch")
       (:file "log")))
     (:module "xmpp"
      :components
      ((:file "engine")
       (:file "handlers")
       (:file "reconnect")))
     (:module "ui"
      :components
      ((:file "classes")
       (:file "theme")
       (:file "layout")
       (:file "keymap")
       (:module "widgets"
        :components
        ((:file "roster")
         (:file "bufferbar")
         (:file "chat")
         (:file "status")
         (:file "input")))
       (:file "tui")))
     (:module "app"
      :components
      ((:file "main")))))))
