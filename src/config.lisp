;;;; config.lisp - Configuration handling for CLabber

(in-package #:clabber)

;;; ============================================================
;;; Configuration Paths
;;; ============================================================

(defvar *config-dir* (merge-pathnames ".config/clabber/" (user-homedir-pathname))
  "Directory for CLabber configuration files.")

(defvar *config-file* (merge-pathnames "config.lisp" *config-dir*)
  "Main configuration file path.")

(defvar *authinfo-file* (merge-pathnames ".authinfo" (user-homedir-pathname))
  "Plain text authinfo file.")

(defvar *authinfo-gpg-file* (merge-pathnames ".authinfo.gpg" (user-homedir-pathname))
  "GPG-encrypted authinfo file.")

;;; ============================================================
;;; Account Configuration Class
;;; ============================================================

(defclass account-config ()
  ((name        :initarg :name :accessor account-config-name)
   (jid         :initarg :jid :accessor account-config-jid)
   (password    :initarg :password :accessor account-config-password :initform nil)
   (host        :initarg :host :accessor account-config-host :initform nil)
   (port        :initarg :port :accessor account-config-port :initform 5222)
   (use-tls     :initarg :use-tls :accessor account-config-use-tls-p :initform t)
   (sasl        :initarg :sasl :accessor account-config-sasl :initform nil)  ; :plain, :external, or nil
   (client-cert :initarg :client-cert :accessor account-config-client-cert :initform nil)  ; path to .pem
   (autoconnect :initarg :autoconnect :accessor account-config-autoconnect :initform nil)
   (mucs        :initarg :mucs :accessor account-config-mucs :initform nil))  ; list of MUC JIDs to auto-join
  (:documentation "Configuration for a single XMPP account."))

(defun make-account-config (&rest args)
  "Create an account configuration."
  (apply #'make-instance 'account-config args))

;;; ============================================================
;;; Main Configuration Class
;;; ============================================================

(defclass config ()
  ((accounts :initarg :accounts :accessor config-accounts :initform nil)
   (default-account :initarg :default-account :accessor config-default-account :initform nil)
   (roster-width :initarg :roster-width :accessor config-roster-width :initform 28)
   (auto-reconnect :initarg :auto-reconnect :accessor config-auto-reconnect-p :initform t)
   (auto-open-on-message :initarg :auto-open-on-message :accessor config-auto-open-on-message-p :initform nil)
   (download-dir :initarg :download-dir
                 :accessor config-download-dir
                 :initform (merge-pathnames "Downloads/CLabber/" (user-homedir-pathname)))
   (theme :initarg :theme :accessor config-theme :initform "tokyo-night")))

(defvar *config* nil
  "Global configuration instance.")

(defun make-config ()
  "Create a new configuration instance."
  (make-instance 'config))

(defun ensure-config-dir ()
  "Ensure the configuration directory exists."
  (ensure-directories-exist *config-dir*))

;;; ============================================================
;;; Authinfo Support (.authinfo / .authinfo.gpg)
;;; ============================================================

(defun read-authinfo-content ()
  "Read authinfo content, decrypting .gpg file if needed."
  (cond
    ;; Try encrypted file first
    ((probe-file *authinfo-gpg-file*)
     (handler-case
         (with-output-to-string (out)
           (uiop:run-program (list "gpg" "--quiet" "--decrypt"
                                   (namestring *authinfo-gpg-file*))
                             :output out
                             :error-output nil))
       (error () nil)))
    ;; Fall back to plain text file
    ((probe-file *authinfo-file*)
     (uiop:read-file-string *authinfo-file*))
    (t nil)))

(defun parse-authinfo-line (line)
  "Parse a single authinfo line into a plist.
Format: machine HOST login USER password PASS [port PORT]"
  (let ((tokens (uiop:split-string line :separator '(#\Space #\Tab)))
        (result nil))
    (loop while tokens do
      (let ((key (pop tokens))
            (val (pop tokens)))
        (when (and key val)
          (cond
            ((string-equal key "machine") (setf (getf result :machine) val))
            ((string-equal key "login") (setf (getf result :login) val))
            ((string-equal key "password") (setf (getf result :password) val))
            ((string-equal key "port") (setf (getf result :port) val))))))
    result))

(defun lookup-authinfo (machine &optional login)
  "Look up credentials from authinfo for MACHINE (and optionally LOGIN).
LOGIN can be just username or full JID - both are checked.
Returns plist with :login and :password, or nil if not found."
  (let ((content (read-authinfo-content)))
    (when content
      (dolist (line (uiop:split-string content :separator '(#\Newline)))
        (let ((entry (parse-authinfo-line line)))
          (when (and entry
                     (string-equal (getf entry :machine) machine)
                     (or (null login)
                         (string-equal (getf entry :login) login)
                         ;; Also match if authinfo has full JID
                         (string-equal (getf entry :login)
                                       (format nil "~a@~a" login machine))
                         ;; Or if we have full JID but authinfo has just username
                         (and (position #\@ login)
                              (string-equal (getf entry :login)
                                            (subseq login 0 (position #\@ login))))))
            (return entry)))))))

;;; ============================================================
;;; systemd-creds Support
;;; ============================================================

(defun read-systemd-cred (path)
  "Decrypt a systemd-creds encrypted file and return its contents."
  (handler-case
      (string-trim '(#\Space #\Newline #\Return)
                   (with-output-to-string (out)
                     (uiop:run-program (list "systemd-creds" "decrypt" path "-")
                                       :output out
                                       :error-output nil)))
    (error () nil)))

;;; ============================================================
;;; pass(1) Password Store Support
;;; ============================================================

(defun read-password-store (password-name)
  "Resolve a password value from pass(1)."
  (handler-case
      (string-trim '(#\Space #\Newline #\Return)
                   (with-output-to-string (out)
                     (uiop:run-program (list "pass" password-name)
                                       :output out
                                       :error-output nil)))
    (error () nil)))

;;; ============================================================
;;; Password Resolution (Multiple Sources)
;;; ============================================================

(defun resolve-password (pw &optional server login)
  "Resolve a password value that may be:
   - :authinfo - read from ~/.authinfo or ~/.authinfo.gpg (requires server/login)
   - (:systemd-creds \"/path/to/file.cred\") - decrypt using systemd-creds
   - (:pass \"password-name\") - decrypt using pass(1)
   - plain string - use directly
   - nil - no password"
  (cond
    ;; :authinfo - lookup from authinfo file
    ((eq pw :authinfo)
     (when (and server login)
       (let ((entry (lookup-authinfo server login)))
         (getf entry :password))))

    ;; (:systemd-creds "/path/to/file.cred") - decrypt using systemd-creds
    ((and (listp pw) (eq (first pw) :systemd-creds))
     (let ((cred-path (second pw)))
       (when cred-path
         (read-systemd-cred cred-path))))

    ;; (:pass "password-name") - decrypt using pass(1)
    ((and (listp pw) (eq (first pw) :pass))
     (let ((password-name (second pw)))
       (when password-name
         (read-password-store password-name))))

    ;; Plain string password or nil
    (t pw)))

(defun get-account-password (ac)
  "Get password for account config, resolving from various sources.
   For authinfo lookup, uses JID domain (not connection host) as machine."
  (let* ((jid (account-config-jid ac))
         (jid-domain (when jid (subseq jid (1+ (position #\@ jid)))))
         (login (when jid (subseq jid 0 (position #\@ jid)))))
    (resolve-password (account-config-password ac) jid-domain login)))

;;; ============================================================
;;; Configuration Serialization
;;; ============================================================

(defun account-config-to-plist (ac)
  "Convert account-config to plist for serialization."
  (list :name (account-config-name ac)
        :jid (account-config-jid ac)
        :password (account-config-password ac)
        :host (account-config-host ac)
        :port (account-config-port ac)
        :use-tls (account-config-use-tls-p ac)
        :sasl (account-config-sasl ac)
        :client-cert (account-config-client-cert ac)
        :autoconnect (account-config-autoconnect ac)))

(defun plist-to-account-config (plist)
  "Convert plist to account-config."
  (apply #'make-account-config plist))

(defun config-to-sexp (cfg)
  "Convert config to s-expression for saving."
  `(:clabber-config
    :version 1
    :default-account ,(config-default-account cfg)
    :roster-width ,(config-roster-width cfg)
    :auto-reconnect ,(config-auto-reconnect-p cfg)
    :auto-open-on-message ,(config-auto-open-on-message-p cfg)
    :download-dir ,(namestring (config-download-dir cfg))
    :theme ,(config-theme cfg)
    :accounts ,(mapcar #'account-config-to-plist (config-accounts cfg))))

(defun sexp-to-config (sexp)
  "Parse s-expression into config object."
  (let ((cfg (make-config)))
    (when (and (listp sexp) (eq (car sexp) :clabber-config))
      (let ((plist (cdr sexp)))
        (setf (config-default-account cfg) (getf plist :default-account))
        (when (getf plist :roster-width)
          (setf (config-roster-width cfg) (getf plist :roster-width)))
        (when (getf plist :auto-reconnect)
          (setf (config-auto-reconnect-p cfg) (getf plist :auto-reconnect)))
        (when (getf plist :auto-open-on-message)
          (setf (config-auto-open-on-message-p cfg) (getf plist :auto-open-on-message)))
        (when (getf plist :download-dir)
          (setf (config-download-dir cfg) (pathname (getf plist :download-dir))))
        (when (getf plist :theme)
          (setf (config-theme cfg) (getf plist :theme)))
        (setf (config-accounts cfg)
              (mapcar #'plist-to-account-config (getf plist :accounts)))))
    cfg))

;;; ============================================================
;;; Configuration Validation
;;; ============================================================

(defun validate-account-config (ac)
  "Validate an account config. Returns list of (level message) pairs."
  (let ((issues nil)
        (name (account-config-name ac)))
    ;; Required fields
    (unless (and name (> (length name) 0))
      (push (list :error "Account missing :name") issues))
    (unless (account-config-jid ac)
      (push (list :error (format nil "[~a] Missing :jid" (or name "?"))) issues))
    ;; JID format validation
    (let ((jid (account-config-jid ac)))
      (when (and jid (not (position #\@ jid)))
        (push (list :error (format nil "[~a] Invalid JID format (missing @): ~a" name jid)) issues)))
    ;; Port validation
    (let ((port (account-config-port ac)))
      (when (and port (or (< port 1) (> port 65535)))
        (push (list :error (format nil "[~a] Invalid port: ~d" name port)) issues)))
    ;; SASL validation
    (let ((sasl (account-config-sasl ac)))
      (when sasl
        (unless (member sasl '(:plain :external))
          (push (list :error (format nil "[~a] Invalid SASL type: ~a (use :plain or :external)"
                                    name sasl)) issues))
        (when (and (eq sasl :external) (not (account-config-client-cert ac)))
          (push (list :warning (format nil "[~a] SASL :external requires :client-cert" name)) issues))))
    ;; Client cert validation
    (let ((cert (account-config-client-cert ac)))
      (when (and cert (stringp cert) (not (probe-file cert)))
        (push (list :warning (format nil "[~a] Client cert not found: ~a" name cert)) issues)))
    (nreverse issues)))

(defun validate-config (cfg)
  "Validate entire config. Prints warnings/errors to *error-output*.
   Returns t if valid (no errors), nil if errors found."
  (let ((all-issues nil)
        (has-errors nil))
    ;; Validate each account
    (dolist (ac (config-accounts cfg))
      (let ((issues (validate-account-config ac)))
        (setf all-issues (append all-issues issues))))
    ;; Check for duplicate account names
    (let ((names (mapcar #'account-config-name (config-accounts cfg))))
      (dolist (name names)
        (when (> (count name names :test #'string-equal) 1)
          (push (list :error (format nil "Duplicate account name: ~a" name)) all-issues))))
    ;; Report issues
    (dolist (issue all-issues)
      (let ((level (first issue))
            (msg (second issue)))
        (when (eq level :error)
          (setf has-errors t))
        (format *error-output* "~&[CONFIG ~a] ~a~%"
                (if (eq level :error) "ERROR" "WARNING") msg)))
    (not has-errors)))

;;; ============================================================
;;; Configuration Load/Save
;;; ============================================================

(defun save-config (&optional (cfg *config*))
  "Save config to file."
  (ensure-config-dir)
  (with-open-file (out *config-file* :direction :output :if-exists :supersede)
    (let ((*print-pretty* t)
          (*print-case* :downcase))
      (prin1 (config-to-sexp cfg) out)
      (terpri out)))
  cfg)

(defun load-config ()
  "Load config from file, or return empty config if not found."
  (setf *config*
        (if (probe-file *config-file*)
            (handler-case
                (with-open-file (in *config-file* :direction :input)
                  (let ((cfg (sexp-to-config (read in nil nil))))
                    (validate-config cfg)
                    cfg))
              (error (e)
                (format *error-output* "Warning: Failed to load config: ~a~%" e)
                (make-config)))
            (make-config)))
  *config*)

;;; ============================================================
;;; Account Lookup Helpers
;;; ============================================================

(defun find-account-config (cfg name)
  "Find account config by name."
  (find name (config-accounts cfg) :key #'account-config-name :test #'string-equal))

(defun add-account-config (cfg ac)
  "Add or update account config."
  (let ((existing (find-account-config cfg (account-config-name ac))))
    (if existing
        (setf (config-accounts cfg)
              (substitute ac existing (config-accounts cfg)))
        (push ac (config-accounts cfg))))
  cfg)

(defun default-account (cfg)
  "Get the default account config, or first account if no default set."
  (or (find-account-config cfg (config-default-account cfg))
      (first (config-accounts cfg))))
