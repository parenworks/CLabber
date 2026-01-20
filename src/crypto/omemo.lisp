;;;; omemo.lisp - OMEMO encryption support (XEP-0384)
;;;;
;;;; Implements the Signal Protocol using Ironclad for crypto primitives.
;;;; This module handles key generation, storage, and the core crypto operations.

(in-package #:clabber)

;;; ============================================================
;;; Constants
;;; ============================================================

(defparameter +omemo-ns+ "urn:xmpp:omemo:2"
  "OMEMO namespace (XEP-0384 version 0.8+)")

(defparameter +omemo-devices-node+ "urn:xmpp:omemo:2:devices"
  "PEP node for device list")

(defparameter +omemo-bundles-node+ "urn:xmpp:omemo:2:bundles"
  "PEP node prefix for key bundles")

(defparameter +prekey-count+ 100
  "Number of one-time prekeys to generate")

;;; ============================================================
;;; Key Classes
;;; ============================================================

(defclass omemo-identity-key ()
  ((private-key :initarg :private-key :accessor identity-private-key
                :documentation "Ed25519 private key (32 bytes)")
   (public-key :initarg :public-key :accessor identity-public-key
               :documentation "Ed25519 public key (32 bytes)"))
  (:documentation "Long-term identity key pair for OMEMO."))

(defclass omemo-signed-prekey ()
  ((id :initarg :id :accessor signed-prekey-id
       :documentation "Signed prekey ID (integer)")
   (private-key :initarg :private-key :accessor signed-prekey-private
                :documentation "X25519 private key")
   (public-key :initarg :public-key :accessor signed-prekey-public
               :documentation "X25519 public key")
   (signature :initarg :signature :accessor signed-prekey-signature
              :documentation "Ed25519 signature of the public key")
   (timestamp :initarg :timestamp :accessor signed-prekey-timestamp
              :documentation "Creation timestamp"))
  (:documentation "Signed prekey for X3DH key agreement."))

(defclass omemo-prekey ()
  ((id :initarg :id :accessor prekey-id
       :documentation "Prekey ID (integer)")
   (private-key :initarg :private-key :accessor prekey-private
                :documentation "X25519 private key")
   (public-key :initarg :public-key :accessor prekey-public
               :documentation "X25519 public key"))
  (:documentation "One-time prekey for X3DH."))

(defclass omemo-device ()
  ((device-id :initarg :device-id :accessor device-id
              :documentation "Unique device ID (random 32-bit integer)")
   (identity-key :initarg :identity-key :accessor device-identity-key
                 :documentation "Long-term identity key pair")
   (signed-prekey :initarg :signed-prekey :accessor device-signed-prekey
                  :documentation "Current signed prekey")
   (prekeys :initarg :prekeys :accessor device-prekeys
            :documentation "Hash table of prekey-id -> omemo-prekey"))
  (:documentation "Local OMEMO device with all key material."))

;;; ============================================================
;;; Key Generation
;;; ============================================================

(defun generate-random-bytes (n)
  "Generate N cryptographically random bytes."
  (ironclad:random-data n))

(defun generate-device-id ()
  "Generate a random 32-bit device ID."
  (let ((bytes (generate-random-bytes 4)))
    (logand (ironclad:octets-to-integer bytes) #x7FFFFFFF)))

(defun generate-x25519-keypair ()
  "Generate an X25519 key pair for Diffie-Hellman.
   Returns (values private-key public-key) as octet vectors."
  (multiple-value-bind (priv pub) (ironclad:generate-key-pair :curve25519)
    (let ((priv-plist (ironclad:destructure-private-key priv))
          (pub-plist (ironclad:destructure-public-key pub)))
      (values (getf priv-plist :x)
              (getf pub-plist :y)))))

(defun generate-ed25519-keypair ()
  "Generate an Ed25519 key pair for signatures.
   Returns (values private-key public-key) as octet vectors."
  (multiple-value-bind (priv pub) (ironclad:generate-key-pair :ed25519)
    (let ((priv-plist (ironclad:destructure-private-key priv))
          (pub-plist (ironclad:destructure-public-key pub)))
      (values (getf priv-plist :x)
              (getf pub-plist :y)))))

(defun sign-ed25519 (private-key message)
  "Sign MESSAGE with Ed25519 PRIVATE-KEY. Returns signature as octet vector."
  (let ((signing-key (ironclad:make-private-key :ed25519 :x private-key)))
    (ironclad:sign-message signing-key message)))

(defun verify-ed25519 (public-key message signature)
  "Verify Ed25519 SIGNATURE on MESSAGE with PUBLIC-KEY. Returns T if valid."
  (let ((verify-key (ironclad:make-public-key :ed25519 :y public-key)))
    (ironclad:verify-signature verify-key message signature)))

(defun generate-identity-key ()
  "Generate a new OMEMO identity key pair."
  (multiple-value-bind (priv pub) (generate-ed25519-keypair)
    (make-instance 'omemo-identity-key
                   :private-key priv
                   :public-key pub)))

(defun generate-signed-prekey (identity-key &optional (id 1))
  "Generate a signed prekey, signed by IDENTITY-KEY."
  (multiple-value-bind (priv pub) (generate-x25519-keypair)
    (let ((signature (sign-ed25519 (identity-private-key identity-key) pub)))
      (make-instance 'omemo-signed-prekey
                     :id id
                     :private-key priv
                     :public-key pub
                     :signature signature
                     :timestamp (get-universal-time)))))

(defun generate-prekey (id)
  "Generate a one-time prekey with given ID."
  (multiple-value-bind (priv pub) (generate-x25519-keypair)
    (make-instance 'omemo-prekey
                   :id id
                   :private-key priv
                   :public-key pub)))

(defun generate-prekeys (count &optional (start-id 1))
  "Generate COUNT one-time prekeys starting at START-ID.
   Returns a hash table of id -> prekey."
  (let ((prekeys (make-hash-table)))
    (loop for i from start-id below (+ start-id count)
          do (setf (gethash i prekeys) (generate-prekey i)))
    prekeys))

(defun generate-omemo-device ()
  "Generate a complete OMEMO device with all key material."
  (let* ((device-id (generate-device-id))
         (identity-key (generate-identity-key))
         (signed-prekey (generate-signed-prekey identity-key))
         (prekeys (generate-prekeys +prekey-count+)))
    (make-instance 'omemo-device
                   :device-id device-id
                   :identity-key identity-key
                   :signed-prekey signed-prekey
                   :prekeys prekeys)))

;;; ============================================================
;;; Key Serialization (for storage and PEP publishing)
;;; ============================================================

(defun bytes-to-base64 (bytes)
  "Encode octet vector to base64 string."
  (cl-base64:usb8-array-to-base64-string bytes))

(defun base64-to-bytes (string)
  "Decode base64 string to octet vector."
  (cl-base64:base64-string-to-usb8-array string))

(defun serialize-identity-key (ik)
  "Serialize identity key to alist for storage."
  `((:private . ,(bytes-to-base64 (identity-private-key ik)))
    (:public . ,(bytes-to-base64 (identity-public-key ik)))))

(defun deserialize-identity-key (alist)
  "Deserialize identity key from alist."
  (make-instance 'omemo-identity-key
                 :private-key (base64-to-bytes (cdr (assoc :private alist)))
                 :public-key (base64-to-bytes (cdr (assoc :public alist)))))

(defun serialize-signed-prekey (spk)
  "Serialize signed prekey to alist for storage."
  `((:id . ,(signed-prekey-id spk))
    (:private . ,(bytes-to-base64 (signed-prekey-private spk)))
    (:public . ,(bytes-to-base64 (signed-prekey-public spk)))
    (:signature . ,(bytes-to-base64 (signed-prekey-signature spk)))
    (:timestamp . ,(signed-prekey-timestamp spk))))

(defun deserialize-signed-prekey (alist)
  "Deserialize signed prekey from alist."
  (make-instance 'omemo-signed-prekey
                 :id (cdr (assoc :id alist))
                 :private-key (base64-to-bytes (cdr (assoc :private alist)))
                 :public-key (base64-to-bytes (cdr (assoc :public alist)))
                 :signature (base64-to-bytes (cdr (assoc :signature alist)))
                 :timestamp (cdr (assoc :timestamp alist))))

(defun serialize-prekey (pk)
  "Serialize prekey to alist for storage."
  `((:id . ,(prekey-id pk))
    (:private . ,(bytes-to-base64 (prekey-private pk)))
    (:public . ,(bytes-to-base64 (prekey-public pk)))))

(defun deserialize-prekey (alist)
  "Deserialize prekey from alist."
  (make-instance 'omemo-prekey
                 :id (cdr (assoc :id alist))
                 :private-key (base64-to-bytes (cdr (assoc :private alist)))
                 :public-key (base64-to-bytes (cdr (assoc :public alist)))))

(defun serialize-omemo-device (device)
  "Serialize complete OMEMO device to alist for storage."
  (let ((prekeys-list nil))
    (maphash (lambda (id pk)
               (declare (ignore id))
               (push (serialize-prekey pk) prekeys-list))
             (device-prekeys device))
    `((:device-id . ,(device-id device))
      (:identity-key . ,(serialize-identity-key (device-identity-key device)))
      (:signed-prekey . ,(serialize-signed-prekey (device-signed-prekey device)))
      (:prekeys . ,prekeys-list))))

(defun deserialize-omemo-device (alist)
  "Deserialize OMEMO device from alist."
  (let ((prekeys (make-hash-table)))
    (dolist (pk-alist (cdr (assoc :prekeys alist)))
      (let ((pk (deserialize-prekey pk-alist)))
        (setf (gethash (prekey-id pk) prekeys) pk)))
    (make-instance 'omemo-device
                   :device-id (cdr (assoc :device-id alist))
                   :identity-key (deserialize-identity-key (cdr (assoc :identity-key alist)))
                   :signed-prekey (deserialize-signed-prekey (cdr (assoc :signed-prekey alist)))
                   :prekeys prekeys)))

;;; ============================================================
;;; Key Storage
;;; ============================================================

(defvar *omemo-device* nil
  "Current OMEMO device for this client instance.")

(defun omemo-keys-path ()
  "Return path to OMEMO keys file."
  (merge-pathnames "omemo-keys.lisp" *config-dir*))

(defun save-omemo-device (device)
  "Save OMEMO device keys to disk."
  (let ((path (omemo-keys-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
            (*print-pretty* t))
        (prin1 (serialize-omemo-device device) out)))
    (debug-log "Saved OMEMO keys to ~a" path)
    device))

(defun load-omemo-device ()
  "Load OMEMO device keys from disk, or generate new ones."
  (let ((path (omemo-keys-path)))
    (if (probe-file path)
        (handler-case
            (with-open-file (in path :direction :input)
              (let ((data (read in)))
                (debug-log "Loaded OMEMO keys from ~a" path)
                (deserialize-omemo-device data)))
          (error (e)
            (debug-log "Error loading OMEMO keys: ~a, generating new" e)
            (let ((device (generate-omemo-device)))
              (save-omemo-device device)
              device)))
        (progn
          (debug-log "No OMEMO keys found, generating new device")
          (let ((device (generate-omemo-device)))
            (save-omemo-device device)
            device)))))

(defun init-omemo ()
  "Initialize OMEMO subsystem, loading or generating keys."
  (setf *omemo-device* (load-omemo-device))
  (debug-log "OMEMO initialized with device ID ~a" (device-id *omemo-device*))
  *omemo-device*)

;;; ============================================================
;;; Bundle Generation (for PEP publishing)
;;; ============================================================

(defun device-bundle-xml (device)
  "Generate XML element for device's key bundle (for PEP publishing)."
  (let ((ik (device-identity-key device))
        (spk (device-signed-prekey device)))
    `(("bundle" . (("xmlns" . ,+omemo-ns+)))
      (("ik" . nil) ,(bytes-to-base64 (identity-public-key ik)))
      (("spk" . (("id" . ,(princ-to-string (signed-prekey-id spk)))))
       ,(bytes-to-base64 (signed-prekey-public spk)))
      (("spks" . nil) ,(bytes-to-base64 (signed-prekey-signature spk)))
      ,@(let ((pks nil))
          (maphash (lambda (id pk)
                     (push `(("pk" . (("id" . ,(princ-to-string id))))
                             ,(bytes-to-base64 (prekey-public pk)))
                           pks))
                   (device-prekeys device))
          pks))))
