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

;;; ============================================================
;;; PEP Publishing (XEP-0163)
;;; ============================================================

(defparameter +ns-pubsub+ "http://jabber.org/protocol/pubsub"
  "PubSub namespace")

(defun make-device-list-xml (device-ids)
  "Create XML for device list publication.
   DEVICE-IDS is a list of integer device IDs."
  (make-xml-element "devices"
                    :namespace +omemo-ns+
                    :children (mapcar (lambda (id)
                                        (make-xml-element "device"
                                                          :attributes `(("id" . ,(princ-to-string id)))))
                                      device-ids)))

(defun make-bundle-xml (device)
  "Create XML for key bundle publication."
  (let ((ik (device-identity-key device))
        (spk (device-signed-prekey device))
        (prekey-elements nil))
    ;; Build prekey elements
    (maphash (lambda (id pk)
               (push (make-xml-element "pk"
                                       :attributes `(("id" . ,(princ-to-string id)))
                                       :text (bytes-to-base64 (prekey-public pk)))
                     prekey-elements))
             (device-prekeys device))
    ;; Build bundle element
    (make-xml-element "bundle"
                      :namespace +omemo-ns+
                      :children (list
                                 (make-xml-element "ik"
                                                   :text (bytes-to-base64 (identity-public-key ik)))
                                 (make-xml-element "spk"
                                                   :attributes `(("id" . ,(princ-to-string (signed-prekey-id spk))))
                                                   :text (bytes-to-base64 (signed-prekey-public spk)))
                                 (make-xml-element "spks"
                                                   :text (bytes-to-base64 (signed-prekey-signature spk)))
                                 (make-xml-element "prekeys"
                                                   :children prekey-elements)))))

(defun make-pep-publish-iq (node item-id payload)
  "Create IQ stanza for PEP publication.
   NODE is the PEP node name, ITEM-ID is the item identifier, PAYLOAD is the XML element."
  (let* ((item (make-xml-element "item"
                                 :attributes `(("id" . ,item-id))
                                 :children (list payload)))
         (publish (make-xml-element "publish"
                                    :attributes `(("node" . ,node))
                                    :children (list item)))
         (pubsub (make-xml-element "pubsub"
                                   :namespace +ns-pubsub+
                                   :children (list publish))))
    (make-iq-stanza "set" :query pubsub :id (format nil "pep-~a" (random 100000)))))

(defun publish-omemo-device-list (conn device-ids)
  "Publish OMEMO device list to PEP.
   DEVICE-IDS is a list of our device IDs."
  (let* ((devices-xml (make-device-list-xml device-ids))
         (iq (make-pep-publish-iq +omemo-devices-node+ "current" devices-xml)))
    (debug-log "Publishing OMEMO device list: ~a" device-ids)
    (xmpp-send conn iq)))

(defun publish-omemo-bundle (conn device)
  "Publish OMEMO key bundle to PEP for DEVICE."
  (let* ((device-id (device-id device))
         (bundle-xml (make-bundle-xml device))
         (node (format nil "~a:~a" +omemo-bundles-node+ device-id))
         (iq (make-pep-publish-iq node (princ-to-string device-id) bundle-xml)))
    (debug-log "Publishing OMEMO bundle for device ~a" device-id)
    (xmpp-send conn iq)))

(defun publish-omemo-keys (conn device)
  "Publish both device list and bundle for DEVICE."
  (publish-omemo-device-list conn (list (device-id device)))
  (publish-omemo-bundle conn device))

;;; ============================================================
;;; Fetching Others' Keys
;;; ============================================================

(defun make-pep-items-iq (to node)
  "Create IQ stanza to fetch PEP items from TO at NODE."
  (let* ((items (make-xml-element "items"
                                  :attributes `(("node" . ,node))))
         (pubsub (make-xml-element "pubsub"
                                   :namespace +ns-pubsub+
                                   :children (list items))))
    (make-iq-stanza "get" :to to :query pubsub :id (format nil "pep-get-~a" (random 100000)))))

(defun fetch-omemo-device-list (conn jid)
  "Request OMEMO device list for JID."
  (let ((iq (make-pep-items-iq jid +omemo-devices-node+)))
    (debug-log "Fetching OMEMO device list for ~a" jid)
    (xmpp-send conn iq)))

(defun fetch-omemo-bundle (conn jid device-id)
  "Request OMEMO bundle for JID's DEVICE-ID."
  (let* ((node (format nil "~a:~a" +omemo-bundles-node+ device-id))
         (iq (make-pep-items-iq jid node)))
    (debug-log "Fetching OMEMO bundle for ~a device ~a" jid device-id)
    (xmpp-send conn iq)))

;;; ============================================================
;;; Parsing Received Keys
;;; ============================================================

(defclass remote-bundle ()
  ((jid :initarg :jid :accessor bundle-jid
        :documentation "JID of the bundle owner")
   (device-id :initarg :device-id :accessor bundle-device-id
              :documentation "Device ID")
   (identity-key :initarg :identity-key :accessor bundle-identity-key
                 :documentation "Ed25519 public key (bytes)")
   (signed-prekey-id :initarg :signed-prekey-id :accessor bundle-signed-prekey-id)
   (signed-prekey :initarg :signed-prekey :accessor bundle-signed-prekey
                  :documentation "X25519 public key (bytes)")
   (signed-prekey-sig :initarg :signed-prekey-sig :accessor bundle-signed-prekey-sig
                      :documentation "Signature (bytes)")
   (prekeys :initarg :prekeys :accessor bundle-prekeys
            :documentation "Hash table of id -> X25519 public key (bytes)"))
  (:documentation "Remote user's OMEMO key bundle."))

(defvar *remote-bundles* (make-hash-table :test 'equal)
  "Cache of remote bundles: (jid . device-id) -> remote-bundle")

(defvar *remote-device-lists* (make-hash-table :test 'equal)
  "Cache of remote device lists: jid -> list of device-ids")

(defun parse-device-list-xml (xml-element)
  "Parse device list from PEP XML. Returns list of device IDs."
  (let ((devices-el (or (xml-child xml-element "devices")
                        xml-element))
        (device-ids nil))
    (when devices-el
      (dolist (child (xml-children devices-el))
        (when (and (listp child) 
                   (string= (xml-name child) "device"))
          (let ((id-str (xml-attr child "id")))
            (when id-str
              (push (parse-integer id-str) device-ids))))))
    (nreverse device-ids)))

(defun parse-bundle-xml (xml-element jid device-id)
  "Parse key bundle from PEP XML. Returns remote-bundle instance."
  (let ((bundle-el (or (xml-child xml-element "bundle")
                       xml-element)))
    (when bundle-el
      (let ((ik-el (xml-child bundle-el "ik"))
            (spk-el (xml-child bundle-el "spk"))
            (spks-el (xml-child bundle-el "spks"))
            (prekeys-el (xml-child bundle-el "prekeys"))
            (prekeys (make-hash-table)))
        ;; Parse prekeys
        (when prekeys-el
          (dolist (child (xml-children prekeys-el))
            (when (and (listp child)
                       (string= (xml-name child) "pk"))
              (let ((id-str (xml-attr child "id"))
                    (key-b64 (xml-text child)))
                (when (and id-str key-b64)
                  (setf (gethash (parse-integer id-str) prekeys)
                        (base64-to-bytes key-b64)))))))
        ;; Build bundle
        (when (and ik-el spk-el spks-el)
          (make-instance 'remote-bundle
                         :jid jid
                         :device-id device-id
                         :identity-key (base64-to-bytes (xml-text ik-el))
                         :signed-prekey-id (parse-integer (xml-attr spk-el "id"))
                         :signed-prekey (base64-to-bytes (xml-text spk-el))
                         :signed-prekey-sig (base64-to-bytes (xml-text spks-el))
                         :prekeys prekeys))))))

(defun cache-device-list (jid device-ids)
  "Cache device list for JID."
  (setf (gethash jid *remote-device-lists*) device-ids)
  (debug-log "Cached device list for ~a: ~a" jid device-ids))

(defun cache-bundle (bundle)
  "Cache remote bundle."
  (let ((key (cons (bundle-jid bundle) (bundle-device-id bundle))))
    (setf (gethash key *remote-bundles*) bundle)
    (debug-log "Cached bundle for ~a device ~a" 
               (bundle-jid bundle) (bundle-device-id bundle))))

(defun get-cached-device-list (jid)
  "Get cached device list for JID, or NIL."
  (gethash jid *remote-device-lists*))

(defun get-cached-bundle (jid device-id)
  "Get cached bundle for JID and DEVICE-ID, or NIL."
  (gethash (cons jid device-id) *remote-bundles*))

;;; ============================================================
;;; X3DH Key Agreement (Signal Protocol)
;;; ============================================================

(defun x25519-dh (private-key public-key)
  "Perform X25519 Diffie-Hellman. Returns shared secret (32 bytes)."
  (let ((priv (ironclad:make-private-key :curve25519 :x private-key :y (make-array 32 :element-type '(unsigned-byte 8))))
        (pub (ironclad:make-public-key :curve25519 :y public-key)))
    (ironclad:diffie-hellman priv pub)))

(defun hkdf-sha256 (input-key-material salt info length)
  "HKDF-SHA256 key derivation.
   Returns LENGTH bytes derived from IKM using SALT and INFO."
  (let ((kdf (ironclad:make-kdf :hkdf :digest :sha256)))
    (ironclad:derive-key kdf input-key-material salt length info)))

(defun concat-bytes (&rest byte-arrays)
  "Concatenate multiple byte arrays into one."
  (let* ((total-len (reduce #'+ byte-arrays :key #'length))
         (result (make-array total-len :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (arr byte-arrays result)
      (replace result arr :start1 pos)
      (incf pos (length arr)))))

(defun x3dh-initiator (our-identity-key our-ephemeral-key
                       their-identity-key their-signed-prekey their-one-time-prekey)
  "Perform X3DH as initiator (Alice).
   OUR-IDENTITY-KEY: our Ed25519 private key (will derive X25519)
   OUR-EPHEMERAL-KEY: freshly generated X25519 private key
   THEIR-IDENTITY-KEY: their Ed25519 public key
   THEIR-SIGNED-PREKEY: their X25519 signed prekey public
   THEIR-ONE-TIME-PREKEY: their X25519 one-time prekey public (or NIL)
   
   Returns shared secret (32 bytes) for initializing Double Ratchet."
  ;; Convert Ed25519 identity keys to X25519 for DH
  ;; Note: In OMEMO, identity keys are Ed25519 but we need X25519 for DH
  ;; The conversion is: x25519_pub = ed25519_pub with sign bit cleared
  ;; For simplicity, we'll use the raw bytes (proper impl would convert)
  (let* (;; DH1 = DH(IKa, SPKb) - our identity with their signed prekey
         (dh1 (x25519-dh our-identity-key their-signed-prekey))
         ;; DH2 = DH(EKa, IKb) - our ephemeral with their identity
         (dh2 (x25519-dh our-ephemeral-key their-identity-key))
         ;; DH3 = DH(EKa, SPKb) - our ephemeral with their signed prekey
         (dh3 (x25519-dh our-ephemeral-key their-signed-prekey))
         ;; DH4 = DH(EKa, OPKb) - our ephemeral with their one-time prekey (if present)
         (dh4 (when their-one-time-prekey
                (x25519-dh our-ephemeral-key their-one-time-prekey)))
         ;; Concatenate all DH outputs
         (dh-concat (if dh4
                        (concat-bytes dh1 dh2 dh3 dh4)
                        (concat-bytes dh1 dh2 dh3)))
         ;; Derive shared secret using HKDF
         (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (info (babel:string-to-octets "OMEMO X3DH")))
    (hkdf-sha256 dh-concat salt info 32)))

(defun x3dh-responder (our-identity-key our-signed-prekey our-one-time-prekey
                       their-identity-key their-ephemeral-key)
  "Perform X3DH as responder (Bob).
   Returns shared secret (32 bytes) matching what initiator computed."
  (let* (;; DH1 = DH(SPKb, IKa) - our signed prekey with their identity
         (dh1 (x25519-dh our-signed-prekey their-identity-key))
         ;; DH2 = DH(IKb, EKa) - our identity with their ephemeral
         (dh2 (x25519-dh our-identity-key their-ephemeral-key))
         ;; DH3 = DH(SPKb, EKa) - our signed prekey with their ephemeral
         (dh3 (x25519-dh our-signed-prekey their-ephemeral-key))
         ;; DH4 = DH(OPKb, EKa) - our one-time prekey with their ephemeral (if used)
         (dh4 (when our-one-time-prekey
                (x25519-dh our-one-time-prekey their-ephemeral-key)))
         ;; Concatenate all DH outputs
         (dh-concat (if dh4
                        (concat-bytes dh1 dh2 dh3 dh4)
                        (concat-bytes dh1 dh2 dh3)))
         ;; Derive shared secret using HKDF
         (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (info (babel:string-to-octets "OMEMO X3DH")))
    (hkdf-sha256 dh-concat salt info 32)))

;;; ============================================================
;;; Session Management
;;; ============================================================

(defclass omemo-session ()
  ((jid :initarg :jid :accessor session-jid)
   (device-id :initarg :device-id :accessor session-device-id)
   (shared-secret :initarg :shared-secret :accessor session-shared-secret
                  :documentation "32-byte shared secret from X3DH")
   (root-key :initarg :root-key :accessor session-root-key
             :documentation "Current root key for Double Ratchet")
   (sending-chain-key :initarg :sending-chain-key :accessor session-sending-chain-key)
   (receiving-chain-key :initarg :receiving-chain-key :accessor session-receiving-chain-key)
   (sending-ratchet-key :initarg :sending-ratchet-key :accessor session-sending-ratchet-key
                        :documentation "Our current DH ratchet key pair")
   (receiving-ratchet-key :initarg :receiving-ratchet-key :accessor session-receiving-ratchet-key
                          :documentation "Their current DH ratchet public key")
   (send-count :initform 0 :accessor session-send-count)
   (recv-count :initform 0 :accessor session-recv-count))
  (:documentation "OMEMO session state for Double Ratchet."))

(defvar *omemo-sessions* (make-hash-table :test 'equal)
  "Active OMEMO sessions: (jid . device-id) -> omemo-session")

(defun get-session (jid device-id)
  "Get existing session or NIL."
  (gethash (cons jid device-id) *omemo-sessions*))

(defun store-session (session)
  "Store session in cache."
  (setf (gethash (cons (session-jid session) (session-device-id session))
                 *omemo-sessions*)
        session))

(defun create-session-as-initiator (jid device-id bundle)
  "Create new OMEMO session as initiator using remote BUNDLE.
   Returns the new session and the ephemeral public key to send."
  ;; Generate ephemeral key pair for this session
  (multiple-value-bind (eph-priv eph-pub) (generate-x25519-keypair)
    ;; Pick a random one-time prekey from their bundle
    (let* ((prekeys (bundle-prekeys bundle))
           (prekey-ids (let ((ids nil))
                         (maphash (lambda (k v) (declare (ignore v)) (push k ids)) prekeys)
                         ids))
           (chosen-prekey-id (when prekey-ids (nth (random (length prekey-ids)) prekey-ids)))
           (chosen-prekey (when chosen-prekey-id (gethash chosen-prekey-id prekeys)))
           ;; Perform X3DH
           ;; Note: Need to convert Ed25519 identity to X25519 - simplified here
           (shared-secret (x3dh-initiator 
                           (identity-private-key (device-identity-key *omemo-device*))
                           eph-priv
                           (bundle-identity-key bundle)
                           (bundle-signed-prekey bundle)
                           chosen-prekey))
           ;; Initialize session
           (session (make-instance 'omemo-session
                                   :jid jid
                                   :device-id device-id
                                   :shared-secret shared-secret
                                   :root-key shared-secret
                                   :sending-chain-key nil
                                   :receiving-chain-key nil
                                   :sending-ratchet-key (cons eph-priv eph-pub)
                                   :receiving-ratchet-key (bundle-signed-prekey bundle))))
      (store-session session)
      (debug-log "Created OMEMO session with ~a device ~a" jid device-id)
      (values session eph-pub chosen-prekey-id))))

;;; ============================================================
;;; Double Ratchet Algorithm
;;; ============================================================

(defun kdf-chain (chain-key)
  "Derive next chain key and message key from current chain key.
   Returns (values next-chain-key message-key)."
  (let* ((info-ck (babel:string-to-octets "chain"))
         (info-mk (babel:string-to-octets "message"))
         (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (next-ck (hkdf-sha256 chain-key salt info-ck 32))
         (msg-key (hkdf-sha256 chain-key salt info-mk 32)))
    (values next-ck msg-key)))

(defun kdf-root (root-key dh-output)
  "Derive new root key and chain key from DH output.
   Returns (values new-root-key new-chain-key)."
  (let* ((input (concat-bytes root-key dh-output))
         (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (info (babel:string-to-octets "ratchet"))
         (derived (hkdf-sha256 input salt info 64)))
    (values (subseq derived 0 32)
            (subseq derived 32 64))))

(defun ratchet-encrypt (session plaintext)
  "Encrypt PLAINTEXT using Double Ratchet SESSION.
   Returns (values ciphertext header) where header contains DH ratchet key."
  ;; Step chain forward
  (multiple-value-bind (next-ck msg-key) 
      (kdf-chain (or (session-sending-chain-key session)
                     (session-root-key session)))
    (setf (session-sending-chain-key session) next-ck)
    (incf (session-send-count session))
    ;; Encrypt with AES-256-GCM
    (let* ((nonce (generate-random-bytes 12))
           (cipher (ironclad:make-cipher :aes 
                                         :key msg-key 
                                         :mode :gcm
                                         :initialization-vector nonce))
           (plaintext-bytes (if (stringp plaintext)
                                (babel:string-to-octets plaintext :encoding :utf-8)
                                plaintext))
           (ciphertext (make-array (length plaintext-bytes) 
                                   :element-type '(unsigned-byte 8))))
      (ironclad:encrypt-in-place cipher plaintext-bytes)
      (replace ciphertext plaintext-bytes)
      ;; Get authentication tag
      (let ((tag (ironclad:produce-tag cipher)))
        ;; Build header with our current ratchet public key
        (let ((header `((:dh . ,(cdr (session-sending-ratchet-key session)))
                        (:n . ,(session-send-count session))
                        (:pn . 0))))  ; previous chain length, simplified
          (values (concat-bytes nonce ciphertext tag) header))))))

(defun ratchet-decrypt (session ciphertext header)
  "Decrypt CIPHERTEXT using Double Ratchet SESSION.
   HEADER contains sender's DH ratchet key.
   Returns plaintext string."
  (let ((their-dh (cdr (assoc :dh header))))
    ;; Check if we need to perform DH ratchet step
    (when (and their-dh 
               (not (equalp their-dh (session-receiving-ratchet-key session))))
      ;; Perform DH ratchet
      (let ((dh-out (x25519-dh (car (session-sending-ratchet-key session)) their-dh)))
        (multiple-value-bind (new-root new-chain) 
            (kdf-root (session-root-key session) dh-out)
          (setf (session-root-key session) new-root)
          (setf (session-receiving-chain-key session) new-chain)
          (setf (session-receiving-ratchet-key session) their-dh)
          ;; Generate new sending ratchet key
          (multiple-value-bind (priv pub) (generate-x25519-keypair)
            (let ((dh-out2 (x25519-dh priv their-dh)))
              (multiple-value-bind (new-root2 new-send-chain)
                  (kdf-root (session-root-key session) dh-out2)
                (setf (session-root-key session) new-root2)
                (setf (session-sending-chain-key session) new-send-chain)
                (setf (session-sending-ratchet-key session) (cons priv pub))))))))
    ;; Step receiving chain forward
    (multiple-value-bind (next-ck msg-key)
        (kdf-chain (or (session-receiving-chain-key session)
                       (session-root-key session)))
      (setf (session-receiving-chain-key session) next-ck)
      (incf (session-recv-count session))
      ;; Decrypt with AES-256-GCM
      (let* ((nonce (subseq ciphertext 0 12))
             (tag (subseq ciphertext (- (length ciphertext) 16)))
             (ct (subseq ciphertext 12 (- (length ciphertext) 16)))
             (cipher (ironclad:make-cipher :aes
                                           :key msg-key
                                           :mode :gcm
                                           :initialization-vector nonce))
             (plaintext (make-array (length ct) :element-type '(unsigned-byte 8))))
        (replace plaintext ct)
        (ironclad:decrypt-in-place cipher plaintext)
        ;; Verify tag - compare with produced tag
        (let ((computed-tag (ironclad:produce-tag cipher)))
          (unless (ironclad:constant-time-equal computed-tag tag)
            (error "OMEMO: Authentication failed")))
        (babel:octets-to-string plaintext :encoding :utf-8)))))

;;; ============================================================
;;; OMEMO Message Encryption/Decryption
;;; ============================================================

(defun omemo-encrypt-message (jid message)
  "Encrypt MESSAGE for all devices of JID.
   Returns OMEMO XML element for the encrypted message."
  (let* ((device-ids (get-cached-device-list jid))
         (key-elements nil)
         ;; Generate random message key for this message
         (msg-key (generate-random-bytes 32))
         (nonce (generate-random-bytes 12)))
    ;; Encrypt the actual message with the message key
    (let* ((cipher (ironclad:make-cipher :aes :key msg-key :mode :gcm 
                                         :initialization-vector nonce))
           (plaintext (babel:string-to-octets message :encoding :utf-8))
           (ciphertext (make-array (length plaintext) :element-type '(unsigned-byte 8))))
      (replace ciphertext plaintext)
      (ironclad:encrypt-in-place cipher ciphertext)
      (let ((tag (ironclad:produce-tag cipher))
            (payload (concat-bytes nonce ciphertext tag)))
        ;; For each recipient device, encrypt the message key
        (dolist (device-id device-ids)
          (let ((session (get-session jid device-id)))
            (when session
              ;; Encrypt message key with session
              (multiple-value-bind (encrypted-key header) 
                  (ratchet-encrypt session msg-key)
                (push (make-xml-element "key"
                                        :attributes `(("rid" . ,(princ-to-string device-id)))
                                        :text (bytes-to-base64 encrypted-key))
                      key-elements)))))
        ;; Build OMEMO encrypted element
        (make-xml-element "encrypted"
                          :namespace +omemo-ns+
                          :children (list
                                     (make-xml-element "header"
                                                       :attributes `(("sid" . ,(princ-to-string (device-id *omemo-device*))))
                                                       :children key-elements)
                                     (make-xml-element "payload"
                                                       :text (bytes-to-base64 payload))))))))

(defun omemo-decrypt-message (encrypted-el from-jid)
  "Decrypt OMEMO encrypted message element.
   Returns plaintext string or NIL if decryption fails."
  (handler-case
      (let* ((header-el (xml-child encrypted-el "header"))
             (payload-el (xml-child encrypted-el "payload"))
             (sender-device-id (parse-integer (xml-attr header-el "sid")))
             (my-device-id (device-id *omemo-device*))
             ;; Find key element for our device
             (my-key-el nil))
        (dolist (child (xml-children header-el))
          (when (and (listp child)
                     (string= (xml-name child) "key")
                     (equal (xml-attr child "rid") (princ-to-string my-device-id)))
            (setf my-key-el child)
            (return)))
        (when (and my-key-el payload-el)
          (let* ((encrypted-key (base64-to-bytes (xml-text my-key-el)))
                 (payload (base64-to-bytes (xml-text payload-el)))
                 (session (get-session from-jid sender-device-id)))
            (when session
              ;; Decrypt the message key
              (let* ((header `((:dh . ,(session-receiving-ratchet-key session))
                               (:n . 0)))
                     (msg-key (ratchet-decrypt session encrypted-key header))
                     ;; Decrypt the payload
                     (nonce (subseq payload 0 12))
                     (tag (subseq payload (- (length payload) 16)))
                     (ct (subseq payload 12 (- (length payload) 16)))
                     (cipher (ironclad:make-cipher :aes :key (babel:string-to-octets msg-key)
                                                   :mode :gcm
                                                   :initialization-vector nonce))
                     (plaintext (make-array (length ct) :element-type '(unsigned-byte 8))))
                (replace plaintext ct)
                (ironclad:decrypt-in-place cipher plaintext)
                (let ((computed-tag (ironclad:produce-tag cipher)))
                  (unless (ironclad:constant-time-equal computed-tag tag)
                    (error "OMEMO payload authentication failed")))
                (babel:octets-to-string plaintext :encoding :utf-8))))))
    (error (e)
      (debug-log "OMEMO decryption error: ~a" e)
      nil)))
