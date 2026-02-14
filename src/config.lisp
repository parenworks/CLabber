(in-package #:clabber.config)

;;; Configuration for CLabber
;;; S-expression config file, authinfo.gpg support, multiple auth methods

(defvar *config-dir* (merge-pathnames ".config/clabber/" (user-homedir-pathname)))
(defvar *config-file* (merge-pathnames "config.lisp" *config-dir*))
(defvar *authinfo-file* (merge-pathnames ".authinfo" (user-homedir-pathname)))
(defvar *authinfo-gpg-file* (merge-pathnames ".authinfo.gpg" (user-homedir-pathname)))

;;; Account configuration

(defclass account-config ()
  ((jid :initarg :jid :accessor account-jid)
   (server :initarg :server :accessor account-server :initform nil
           :documentation "Override server (if different from JID domain)")
   (port :initarg :port :accessor account-port :initform 5222)
   (tls-p :initarg :tls :accessor account-tls-p :initform t)
   (auth-method :initarg :auth-method :accessor account-auth-method :initform :password
                :documentation "Auth method: :password, :sasl-plain, :sasl-external, :authinfo")
   (password :initarg :password :accessor account-password :initform nil)
   (client-cert :initarg :client-cert :accessor account-client-cert :initform nil
                :documentation "Path to client certificate for SASL EXTERNAL")
   (autojoin :initarg :autojoin :accessor account-autojoin :initform nil
             :documentation "List of MUC JIDs to auto-join")
   (autoconnect-p :initarg :autoconnect :accessor account-autoconnect-p :initform t))
  (:documentation "XMPP account configuration"))

;;; Main config

(defclass config ()
  ((accounts :initarg :accounts :accessor config-accounts :initform nil)
   (time-format :initarg :time-format :accessor config-time-format :initform "%H:%M")
   (roster-width :initarg :roster-width :accessor config-roster-width :initform 20)
   (participants-width :initarg :participants-width :accessor config-participants-width :initform 18))
  (:documentation "Main CLabber configuration"))

(defun make-config ()
  (make-instance 'config))

(defun ensure-config-dir ()
  (ensure-directories-exist *config-dir*))

(defun account-to-plist (acct)
  (list :jid (account-jid acct)
        :server (account-server acct)
        :port (account-port acct)
        :tls (account-tls-p acct)
        :auth-method (account-auth-method acct)
        :password (account-password acct)
        :client-cert (account-client-cert acct)
        :autojoin (account-autojoin acct)
        :autoconnect (account-autoconnect-p acct)))

(defun plist-to-account (plist)
  "Create account-config from plist, ignoring unknown keys."
  (let ((valid-keys '(:jid :server :port :tls :auth-method :password
                      :client-cert :autojoin :autoconnect))
        (filtered nil))
    (loop for (k v) on plist by #'cddr
          when (member k valid-keys)
          do (push v filtered) (push k filtered))
    (apply #'make-instance 'account-config filtered)))

(defun config-to-sexp (cfg)
  `(:clabber-config
    :version 2
    :time-format ,(config-time-format cfg)
    :roster-width ,(config-roster-width cfg)
    :participants-width ,(config-participants-width cfg)
    :accounts ,(mapcar #'account-to-plist (config-accounts cfg))))

(defun sexp-to-config (sexp)
  (let ((cfg (make-config)))
    (when (and (listp sexp) (eq (car sexp) :clabber-config))
      (let ((plist (cdr sexp)))
        (when (getf plist :time-format)
          (setf (config-time-format cfg) (getf plist :time-format)))
        (when (getf plist :roster-width)
          (setf (config-roster-width cfg) (getf plist :roster-width)))
        (when (getf plist :participants-width)
          (setf (config-participants-width cfg) (getf plist :participants-width)))
        (setf (config-accounts cfg)
              (mapcar #'plist-to-account (getf plist :accounts)))))
    cfg))

(defun save-config (cfg)
  (ensure-config-dir)
  (with-open-file (out *config-file* :direction :output :if-exists :supersede)
    (let ((*print-pretty* t)
          (*print-right-margin* 80))
      (prin1 (config-to-sexp cfg) out)
      (terpri out))))

(defun load-config ()
  (if (probe-file *config-file*)
      (with-open-file (in *config-file* :direction :input)
        (sexp-to-config (read in nil nil)))
      (make-config)))

(defun lookup-authinfo-password (jid)
  "Look up password from .authinfo or .authinfo.gpg for JID."
  (let* ((domain (subseq jid (1+ (position #\@ jid))))
         (user (subseq jid 0 (position #\@ jid)))
         (file (cond
                 ((probe-file *authinfo-gpg-file*)
                  ;; Decrypt with gpg - no --batch so pinentry can prompt
                  (with-output-to-string (s)
                    (let ((proc (sb-ext:run-program "gpg" (list "--quiet" "--decrypt"
                                                                (namestring *authinfo-gpg-file*))
                                                    :output s :error *error-output*
                                                    :input :stream :wait t
                                                    :search t)))
                      (unless (zerop (sb-ext:process-exit-code proc))
                        (return-from lookup-authinfo-password nil)))))
                 ((probe-file *authinfo-file*)
                  (with-open-file (in *authinfo-file*)
                    (with-output-to-string (s)
                      (loop for line = (read-line in nil nil)
                            while line do (write-line line s)))))
                 (t nil))))
    (when file
      (loop for line in (cl-ppcre:split "\\n" file)
            when (and (search domain line) (search user line))
            do (let ((pw-pos (search "password " line)))
                 (when pw-pos
                   (return (string-trim '(#\Space #\Tab #\")
                                        (subseq line (+ pw-pos 9))))))))))
