;;;; main.lisp - Application entrypoint for CLabber

(in-package #:clabber)

(defun main ()
  "Main entry point for CLabber."
  (handler-case
      (progn
        ;; Load configuration
        (load-config)

        ;; Apply theme from config or user theme file
        (apply-config-theme)

        ;; Initialize OMEMO encryption
        (init-omemo)

        ;; Get default account
        (let* ((account (default-account *config*))
               (jid (when account (account-config-jid account)))
               (password (when account (get-account-password account)))
               (host (when account (account-config-host account)))
               (port (when account (account-config-port account)))
               (engine (make-instance 'xmpp-engine)))

          (unless account
            (format *error-output* "~&No accounts configured. Please edit ~/.config/clabber/config.lisp~%")
            (format *error-output* "~&Example config:~%")
            (format *error-output* "~&(:clabber-config~%")
            (format *error-output* "~&  :version 1~%")
            (format *error-output* "~&  :default-account \"main\"~%")
            (format *error-output* "~&  :accounts~%")
            (format *error-output* "~&  ((:name \"main\"~%")
            (format *error-output* "~&    :jid \"user@example.com\"~%")
            (format *error-output* "~&    :password (:pass \"xmpp/example.com\")~%")
            (format *error-output* "~&    :autoconnect t)))~%")
            (return-from main))

          ;; Get MUC rooms from config
          (let ((mucs (when account (account-config-mucs account))))
            
            (when jid
              (engine-start engine jid password
                            :host host
                            :port (or port 5222)
                            :mucs mucs)))

          (unwind-protect
               (ui-run engine)
            (engine-stop engine))))
    (error (e)
      (format *error-output* "~&CLabber error: ~a~%" e))))
