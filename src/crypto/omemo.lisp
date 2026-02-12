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

;; Legacy OMEMO (v0.3) namespace - used by Dino, Conversations, Monal, etc.
(defparameter +omemo-legacy-ns+ "eu.siacs.conversations.axolotl"
  "Legacy OMEMO namespace (v0.3)")

(defparameter +omemo-legacy-devices-node+ "eu.siacs.conversations.axolotl.devicelist"
  "Legacy PEP node for device list")

(defparameter +omemo-legacy-bundles-node+ "eu.siacs.conversations.axolotl.bundles"
  "Legacy PEP node prefix for key bundles")

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
;;; Ed25519 ↔ X25519 Key Conversion
;;; ============================================================
;;;
;;; OMEMO uses Ed25519 for identity keys (signing) but X3DH requires X25519
;;; (Diffie-Hellman). These use different curve representations:
;;; - Ed25519: Twisted Edwards curve, points are (x, y)
;;; - X25519: Montgomery curve (Curve25519), points are just u-coordinate
;;;
;;; The birational map from Edwards to Montgomery is:
;;;   u = (1 + y) / (1 - y)  (mod p)
;;; where p = 2^255 - 19
;;;
;;; For private keys, Ed25519 uses a seed that's hashed with SHA-512,
;;; then clamped. The first 32 bytes become the X25519 scalar.

(defparameter +curve25519-prime+
  (- (expt 2 255) 19)
  "The prime field for Curve25519: p = 2^255 - 19")

(defun bytes-to-integer-le (bytes)
  "Convert little-endian byte array to integer."
  (let ((result 0))
    (loop for i from 0 below (length bytes)
          do (setf result (logior result (ash (aref bytes i) (* 8 i)))))
    result))

(defun integer-to-bytes-le (n &optional (len 32))
  "Convert integer to little-endian byte array of LEN bytes."
  (let ((result (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 0 below len
          do (setf (aref result i) (logand (ash n (* -8 i)) #xff)))
    result))

(defun mod-inverse (a p)
  "Compute modular inverse of A mod P using extended Euclidean algorithm."
  (let ((old-r p) (r a)
        (old-s 0) (s 1))
    (loop while (not (zerop r))
          do (let ((quotient (floor old-r r)))
               (psetf old-r r r (- old-r (* quotient r)))
               (psetf old-s s s (- old-s (* quotient s)))))
    (mod old-s p)))

(defun ed25519-public-to-x25519 (ed25519-pub)
  "Convert Ed25519 public key (32 bytes) to X25519 public key (32 bytes).
   
   Ed25519 public keys encode the y-coordinate (with sign bit for x in MSB).
   The conversion formula is: u = (1 + y) / (1 - y) mod p
   
   Returns X25519 public key as 32-byte array."
  (let* ((p +curve25519-prime+)
         ;; Ed25519 public key is y-coordinate in little-endian, with sign bit in MSB
         ;; Clear the sign bit to get just y
         (y-bytes (copy-seq ed25519-pub))
         (_ (setf (aref y-bytes 31) (logand (aref y-bytes 31) #x7f)))
         (y (bytes-to-integer-le y-bytes))
         ;; u = (1 + y) / (1 - y) mod p
         (numerator (mod (1+ y) p))
         (denominator (mod (- 1 y) p))
         (u (mod (* numerator (mod-inverse denominator p)) p)))
    (declare (ignore _))
    (integer-to-bytes-le u 32)))

(defun ed25519-private-to-x25519 (ed25519-priv)
  "Convert Ed25519 private key (seed) to X25519 private key.
   
   Ed25519 private key is a 32-byte seed. To get the X25519 scalar:
   1. Hash the seed with SHA-512
   2. Take the first 32 bytes
   3. Apply clamping (clear bits 0,1,2,255; set bit 254)
   
   Returns X25519 private key as 32-byte array."
  (let* ((hash (ironclad:digest-sequence :sha512 ed25519-priv))
         (scalar (subseq hash 0 32)))
    ;; Clamp the scalar for X25519
    (setf (aref scalar 0) (logand (aref scalar 0) #xf8))      ; Clear bits 0,1,2
    (setf (aref scalar 31) (logand (aref scalar 31) #x7f))    ; Clear bit 255
    (setf (aref scalar 31) (logior (aref scalar 31) #x40))    ; Set bit 254
    scalar))

;;; ============================================================
;;; XEdDSA Signing (Signal Protocol compatible)
;;; ============================================================
;;; 
;;; XEdDSA allows signing with a Curve25519 (X25519) private key,
;;; producing signatures verifiable by libsignal's verifySignatureCurve.
;;; See: https://signal.org/docs/specifications/xeddsa/
;;;
;;; Uses ironclad's internal Ed25519 primitives from the CRYPTO package:
;;; ec-scalar-mult, ec-encode-point, ec-decode-scalar, ec-encode-scalar,
;;; +ed25519-b+ (base point), +ed25519-l+ (group order), ed25519-hash (SHA-512)

(defun xeddsa-sign (ed25519-seed message)
  "Sign MESSAGE using XEdDSA with the Ed25519 seed (identity private key).
   
   This produces signatures compatible with libsignal's verifySignatureCurve.
   
   Algorithm (from Signal XEdDSA spec):
   1. Derive X25519 scalar k from Ed25519 seed (SHA-512 + clamp)
   2. calculate_key_pair(k): E = kB, if sign bit of E is 1, negate k
   3. Compute nonce r = hash1(a || message || Z) mod L
   4. Compute R = rB
   5. Compute h = hash(R || A || message) mod L
   6. Compute s = (r + h*a) mod L
   7. Return R || s (64 bytes)"
  (let* (;; Step 1: Derive clamped scalar from Ed25519 seed
         (scalar-bytes (ed25519-private-to-x25519 ed25519-seed))
         (k (ironclad::ec-decode-scalar :ed25519 scalar-bytes))
         ;; Step 2: calculate_key_pair(k)
         ;; E = kB (scalar mult of base point)
         (e-point (ironclad::ec-scalar-mult ironclad::+ed25519-b+ k))
         (e-encoded (ironclad::ec-encode-point e-point))
         ;; Check sign bit (MSB of last byte = bit 255 of encoded point)
         (sign-bit (ldb (byte 1 7) (aref e-encoded 31)))
         ;; If sign bit is 1, negate: a = L - k, else a = k
         (a (if (= sign-bit 1)
                (mod (- ironclad::+ed25519-l+ k) ironclad::+ed25519-l+)
                (mod k ironclad::+ed25519-l+)))
         ;; A = public key with sign bit forced to 0
         (a-pub (copy-seq e-encoded)))
    (setf (aref a-pub 31) (logand (aref a-pub 31) #x7f))  ; Clear sign bit
    (let* ((a-bytes (ironclad::ec-encode-scalar :ed25519 a))
           ;; Step 3: Compute nonce r = hash1(a || message || Z) mod L
           ;; hash1 means prefix with 0xFE followed by 31 bytes of 0xFF (32 bytes total)
           (z (generate-random-bytes 64))
           (hash1-prefix (make-array 32 :element-type '(unsigned-byte 8) :initial-element #xff)))
      (setf (aref hash1-prefix 0) #xfe)
      (let* ((r-hash (ironclad::ed25519-hash hash1-prefix a-bytes message z))
             (r-int (mod (ironclad::ec-decode-scalar :ed25519 r-hash) ironclad::+ed25519-l+))
             ;; Step 4: R = rB
             (r-point (ironclad::ec-scalar-mult ironclad::+ed25519-b+ r-int))
             (r-encoded (ironclad::ec-encode-point r-point))
             ;; Step 5: h = hash(R || A || message) mod L
             (h-hash (ironclad::ed25519-hash r-encoded a-pub message))
             (h-int (mod (ironclad::ec-decode-scalar :ed25519 h-hash) ironclad::+ed25519-l+))
             ;; Step 6: s = (r + h*a) mod L
             (s-int (mod (+ r-int (* h-int a)) ironclad::+ed25519-l+))
             (s-bytes (ironclad::ec-encode-scalar :ed25519 s-int)))
        ;; Step 7: Return R || s
        (concatenate '(vector (unsigned-byte 8)) r-encoded s-bytes)))))

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
   NODE is the PEP node name, ITEM-ID is the item identifier, PAYLOAD is the XML element.
   Node access is configured separately via configure-pep-node-open."
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

(defun make-legacy-device-list-xml (device-ids)
  "Create XML for legacy OMEMO device list publication."
  (make-xml-element "list"
                    :namespace +omemo-legacy-ns+
                    :children (mapcar (lambda (id)
                                        (make-xml-element "device"
                                                          :attributes `(("id" . ,(princ-to-string id)))))
                                      device-ids)))

(defun prepend-djb-type (key-bytes)
  "Prepend the 0x05 DJB type byte to a public key for libsignal compatibility.
   libsignal expects all public keys to be 33 bytes: 0x05 || 32-byte-key."
  (let ((result (make-array (1+ (length key-bytes)) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) #x05)
    (replace result key-bytes :start1 1)
    result))

(defun make-legacy-bundle-xml (device)
  "Create XML for legacy OMEMO key bundle publication.
   Legacy OMEMO (v0.3/libsignal) expects:
   - All public keys with 0x05 DJB type prefix (33 bytes)
   - Identity key as X25519 (Curve25519) public key, NOT Ed25519
   - Signed prekey signature made with XEdDSA (not Ed25519)
   The 0x05 prefix causes omemo_dr to parse as CurvePublicKey,
   which uses verifySignatureCurve (XEdDSA verification)."
  (let* ((ik (device-identity-key device))
         (spk (device-signed-prekey device))
         ;; Convert Ed25519 identity public key to X25519 for legacy bundle
         (x25519-identity-pub (ed25519-public-to-x25519 (identity-public-key ik)))
         ;; Re-sign the signed prekey with XEdDSA (compatible with verifySignatureCurve)
         ;; The message being signed is the serialized signed prekey public key WITH 0x05 prefix
         (spk-pub-with-prefix (prepend-djb-type (signed-prekey-public spk)))
         (xeddsa-sig (xeddsa-sign (identity-private-key ik) spk-pub-with-prefix))
         (prekey-elements nil))
    (maphash (lambda (id pk)
               (push (make-xml-element "preKeyPublic"
                                       :attributes `(("preKeyId" . ,(princ-to-string id)))
                                       :text (bytes-to-base64 (prepend-djb-type (prekey-public pk))))
                     prekey-elements))
             (device-prekeys device))
    (make-xml-element "bundle"
                      :namespace +omemo-legacy-ns+
                      :children (list
                                 (make-xml-element "signedPreKeyPublic"
                                                   :attributes `(("signedPreKeyId" . ,(princ-to-string (signed-prekey-id spk))))
                                                   :text (bytes-to-base64 spk-pub-with-prefix))
                                 (make-xml-element "signedPreKeySignature"
                                                   :text (bytes-to-base64 xeddsa-sig))
                                 (make-xml-element "identityKey"
                                                   :text (bytes-to-base64 (prepend-djb-type x25519-identity-pub)))
                                 (make-xml-element "prekeys"
                                                   :children prekey-elements)))))

(defun publish-omemo-device-list (conn device-ids)
  "Publish OMEMO device list to PEP on both modern and legacy namespaces."
  ;; Modern namespace
  (let* ((devices-xml (make-device-list-xml device-ids))
         (iq (make-pep-publish-iq +omemo-devices-node+ "current" devices-xml)))
    (debug-log "Publishing OMEMO device list (modern): ~a" device-ids)
    (xmpp-send conn iq))
  ;; Legacy namespace
  (let* ((devices-xml (make-legacy-device-list-xml device-ids))
         (iq (make-pep-publish-iq +omemo-legacy-devices-node+ "current" devices-xml)))
    (debug-log "Publishing OMEMO device list (legacy): ~a" device-ids)
    (xmpp-send conn iq)))

(defun publish-omemo-bundle (conn device)
  "Publish OMEMO key bundle to PEP on both modern and legacy namespaces."
  (let ((device-id (device-id device)))
    ;; Modern namespace
    (let* ((bundle-xml (make-bundle-xml device))
           (node (format nil "~a:~a" +omemo-bundles-node+ device-id))
           (iq (make-pep-publish-iq node (princ-to-string device-id) bundle-xml)))
      (debug-log "Publishing OMEMO bundle (modern) for device ~a" device-id)
      (xmpp-send conn iq))
    ;; Legacy namespace
    (let* ((bundle-xml (make-legacy-bundle-xml device))
           (node (format nil "~a:~a" +omemo-legacy-bundles-node+ device-id))
           (iq (make-pep-publish-iq node "current" bundle-xml)))
      (debug-log "Publishing OMEMO bundle (legacy) for device ~a" device-id)
      (xmpp-send conn iq))))

(defun configure-pep-node-open (conn node)
  "Configure a PEP node to have open access model.
   This is needed for existing nodes that were created with default (presence) access."
  (let* ((config-form
           (make-xml-element "x"
                             :namespace "jabber:x:data"
                             :attributes '(("type" . "submit"))
                             :children (list
                                        (make-xml-element "field"
                                                          :attributes '(("var" . "FORM_TYPE") ("type" . "hidden"))
                                                          :children (list (make-xml-element "value" :text "http://jabber.org/protocol/pubsub#node_config")))
                                        (make-xml-element "field"
                                                          :attributes '(("var" . "pubsub#access_model"))
                                                          :children (list (make-xml-element "value" :text "open")))
                                        (make-xml-element "field"
                                                          :attributes '(("var" . "pubsub#max_items"))
                                                          :children (list (make-xml-element "value" :text "1"))))))
         (configure (make-xml-element "configure"
                                      :attributes `(("node" . ,node))
                                      :children (list config-form)))
         (pubsub-owner (make-xml-element "pubsub"
                                         :namespace "http://jabber.org/protocol/pubsub#owner"
                                         :children (list configure)))
         (iq (make-iq-stanza "set" :query pubsub-owner :id (format nil "pep-cfg-~a" (random 100000)))))
    (debug-log "Configuring PEP node ~a with open access" node)
    (xmpp-send conn iq)))

(defun publish-omemo-keys (conn device)
  "Publish both device list and bundle for DEVICE on both namespaces.
   Publishes first (creates nodes), then configures open access.
   Merges our device into the existing device list to avoid removing other devices."
  ;; Publish device list (merged with existing)
  (let* ((our-jid (bare-jid (conn-bound-jid conn)))
         (existing (get-cached-device-list our-jid))
         (our-id (device-id device))
         (merged (if existing
                     (remove-duplicates (cons our-id existing))
                     (list our-id))))
    (debug-log "OMEMO: Merging device list for ~a: existing=~a merged=~a" our-jid existing merged)
    (publish-omemo-device-list conn merged))
  ;; Publish bundles
  (publish-omemo-bundle conn device)
  ;; Configure all nodes with open access AFTER publishing (nodes must exist first)
  (let ((device-id (device-id device)))
    (handler-case
        (progn
          (configure-pep-node-open conn +omemo-devices-node+)
          (configure-pep-node-open conn +omemo-legacy-devices-node+)
          (configure-pep-node-open conn (format nil "~a:~a" +omemo-bundles-node+ device-id))
          (configure-pep-node-open conn (format nil "~a:~a" +omemo-legacy-bundles-node+ device-id)))
      (error (e) (debug-log "Error configuring PEP nodes: ~a" e)))))

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
  "Request OMEMO device list for JID from both modern and legacy nodes."
  ;; Modern namespace
  (let ((iq (make-pep-items-iq jid +omemo-devices-node+)))
    (debug-log "Fetching OMEMO device list (modern) for ~a" jid)
    (xmpp-send conn iq))
  ;; Legacy namespace
  (let ((iq (make-pep-items-iq jid +omemo-legacy-devices-node+)))
    (debug-log "Fetching OMEMO device list (legacy) for ~a" jid)
    (xmpp-send conn iq)))

(defun fetch-omemo-bundle (conn jid device-id)
  "Request OMEMO bundle for JID's DEVICE-ID from both modern and legacy nodes."
  ;; Modern namespace
  (let* ((node (format nil "~a:~a" +omemo-bundles-node+ device-id))
         (iq (make-pep-items-iq jid node)))
    (debug-log "Fetching OMEMO bundle (modern) for ~a device ~a" jid device-id)
    (xmpp-send conn iq))
  ;; Legacy namespace
  (let* ((node (format nil "~a:~a" +omemo-legacy-bundles-node+ device-id))
         (iq (make-pep-items-iq jid node)))
    (debug-log "Fetching OMEMO bundle (legacy) for ~a device ~a" jid device-id)
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
  "Parse device list from PEP XML. Handles both modern (<devices>) and legacy (<list>) formats.
   Returns list of device IDs."
  (let ((devices-el (or (xml-child xml-element "devices")
                        (xml-child xml-element "list")
                        xml-element))
        (device-ids nil))
    (when devices-el
      (dolist (child (xml-children devices-el))
        (when (and (typep child 'xml-element)
                   (string= (xml-name child) "device"))
          (let ((id-str (xml-attr child "id")))
            (when id-str
              (push (parse-integer id-str) device-ids))))))
    (nreverse device-ids)))

(defun parse-bundle-xml (xml-element jid device-id)
  "Parse key bundle from PEP XML. Handles both modern and legacy formats.
   Returns remote-bundle instance."
  (let ((bundle-el (or (xml-child xml-element "bundle")
                       xml-element)))
    (when bundle-el
      ;; Try modern format first (ik, spk, spks, prekeys/pk)
      (let ((ik-el (xml-child bundle-el "ik"))
            (spk-el (xml-child bundle-el "spk"))
            (spks-el (xml-child bundle-el "spks"))
            ;; Legacy format (identityKey, signedPreKeyPublic, signedPreKeySignature, prekeys/preKeyPublic)
            (legacy-ik-el (xml-child bundle-el "identityKey"))
            (legacy-spk-el (xml-child bundle-el "signedPreKeyPublic"))
            (legacy-spks-el (xml-child bundle-el "signedPreKeySignature"))
            (prekeys-el (xml-child bundle-el "prekeys"))
            (prekeys (make-hash-table)))
        ;; Determine which format we have
        (let ((is-legacy (and legacy-ik-el (not ik-el))))
          (debug-log "Parsing bundle for ~a device ~a (legacy=~a)" jid device-id is-legacy)
          ;; Parse prekeys - handle both "pk" (modern) and "preKeyPublic" (legacy)
          (when prekeys-el
            (dolist (child (xml-children prekeys-el))
              (when (typep child 'xml-element)
                (let ((is-pk (or (string= (xml-name child) "pk")
                                 (string= (xml-name child) "preKeyPublic"))))
                  (when is-pk
                    (let ((id-str (or (xml-attr child "id")
                                      (xml-attr child "preKeyId")))
                          (key-b64 (xml-text child)))
                      (when (and id-str key-b64)
                        (setf (gethash (parse-integer id-str) prekeys)
                              (base64-to-bytes key-b64)))))))))
          ;; Build bundle from whichever format we found
          (let ((final-ik (or ik-el legacy-ik-el))
                (final-spk (or spk-el legacy-spk-el))
                (final-spks (or spks-el legacy-spks-el)))
            (when (and final-ik final-spk final-spks)
              (let ((spk-id-str (if is-legacy
                                    (xml-attr final-spk "signedPreKeyId")
                                    (xml-attr final-spk "id"))))
                (make-instance 'remote-bundle
                               :jid jid
                               :device-id device-id
                               :identity-key (base64-to-bytes (xml-text final-ik))
                               :signed-prekey-id (parse-integer (or spk-id-str "0"))
                               :signed-prekey (base64-to-bytes (xml-text final-spk))
                               :signed-prekey-sig (base64-to-bytes (xml-text final-spks))
                               :prekeys prekeys)))))))))

(defun cache-device-list (jid device-ids)
  "Cache device list for JID, merging with any existing cached list."
  (let* ((existing (gethash jid *remote-device-lists*))
         (merged (if existing
                     (remove-duplicates (append device-ids existing))
                     device-ids)))
    (setf (gethash jid *remote-device-lists*) merged)
    (debug-log "Cached device list for ~a: ~a" jid merged)))

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
;;; OMEMO Encrypted Message Parsing
;;; ============================================================

(defclass omemo-key-element ()
  ((recipient-device-id :initarg :rid :accessor key-recipient-device-id
                        :documentation "Recipient device ID this key is for")
   (is-prekey :initarg :prekey :accessor key-is-prekey :initform nil
              :documentation "T if this is a prekey message (first message in session)")
   (key-data :initarg :data :accessor key-data
             :documentation "Encrypted key material (bytes)"))
  (:documentation "A <key> element from OMEMO encrypted message header."))

(defclass omemo-message ()
  ((sender-device-id :initarg :sid :accessor omemo-sender-device-id
                     :documentation "Sender's device ID")
   (keys :initarg :keys :accessor omemo-keys
         :documentation "List of omemo-key-element for each recipient device")
   (iv :initarg :iv :accessor omemo-iv
       :documentation "Initialization vector (12 bytes)")
   (payload :initarg :payload :accessor omemo-payload
            :documentation "Encrypted payload (bytes) or NIL for key-only message"))
  (:documentation "Parsed OMEMO encrypted message."))

(defun parse-omemo-encrypted (encrypted-el)
  "Parse an OMEMO <encrypted> element into an omemo-message structure.
   Returns omemo-message instance or NIL on parse failure."
  (handler-case
      (let ((header-el (xml-child encrypted-el "header"))
            (payload-el (xml-child encrypted-el "payload")))
        (when header-el
          (let ((sender-id (parse-integer (xml-attr header-el "sid")))
                (iv-el (xml-child header-el "iv"))
                (keys nil))
            ;; Parse all <key> elements
            (dolist (child (xml-children header-el))
              (when (and (typep child 'xml-element)
                         (string= (xml-name child) "key"))
                (let ((rid-str (xml-attr child "rid"))
                      (prekey-str (xml-attr child "prekey"))
                      (key-b64 (xml-text child)))
                  (when (and rid-str key-b64)
                    (push (make-instance 'omemo-key-element
                                         :rid (parse-integer rid-str)
                                         :prekey (and prekey-str 
                                                      (or (string= prekey-str "true")
                                                          (string= prekey-str "1")))
                                         :data (base64-to-bytes key-b64))
                          keys)))))
            ;; Build the message structure
            (make-instance 'omemo-message
                           :sid sender-id
                           :keys (nreverse keys)
                           :iv (when iv-el (base64-to-bytes (xml-text iv-el)))
                           :payload (when payload-el 
                                      (base64-to-bytes (xml-text payload-el)))))))
    (error (e)
      (debug-log "Error parsing OMEMO encrypted element: ~a" e)
      nil)))

(defun find-key-for-device (omemo-msg device-id)
  "Find the key element intended for DEVICE-ID in OMEMO-MSG.
   Returns omemo-key-element or NIL."
  (find device-id (omemo-keys omemo-msg) :key #'key-recipient-device-id))

;;; ============================================================
;;; Minimal Protobuf Parser (for Signal Protocol wire format)
;;; ============================================================

(defun protobuf-read-varint (data offset)
  "Read a protobuf varint from DATA starting at OFFSET.
   Returns (values integer new-offset)."
  (let ((result 0) (shift 0))
    (loop for i from offset below (length data)
          for byte = (aref data i)
          do (setf result (logior result (ash (logand byte #x7f) shift)))
             (incf shift 7)
             (when (zerop (logand byte #x80))
               (return-from protobuf-read-varint (values result (1+ i)))))
    (values result (length data))))

(defun protobuf-parse-fields (data)
  "Parse protobuf DATA into an alist of (field-number . value).
   Handles varint (wire type 0) and length-delimited (wire type 2).
   Returns alist of (field-number . value)."
  (let ((fields nil)
        (offset 0)
        (len (length data)))
    (loop while (< offset len)
          do (multiple-value-bind (tag new-offset) (protobuf-read-varint data offset)
               (setf offset new-offset)
               (let ((field-number (ash tag -3))
                     (wire-type (logand tag #x07)))
                 (cond
                   ;; Wire type 0: varint
                   ((= wire-type 0)
                    (multiple-value-bind (value next-offset) (protobuf-read-varint data offset)
                      (push (cons field-number value) fields)
                      (setf offset next-offset)))
                   ;; Wire type 2: length-delimited (bytes/string/embedded message)
                   ((= wire-type 2)
                    (multiple-value-bind (length next-offset) (protobuf-read-varint data offset)
                      (setf offset next-offset)
                      (let ((value (subseq data offset (+ offset length))))
                        (push (cons field-number value) fields)
                        (setf offset (+ offset length)))))
                   ;; Skip unknown wire types
                   (t (return))))))
    (nreverse fields)))

;;; ============================================================
;;; Signal Protocol Message Parsing
;;; ============================================================

(defclass signal-prekey-message ()
  ((registration-id :initarg :registration-id :accessor spkm-registration-id)
   (prekey-id :initarg :prekey-id :accessor spkm-prekey-id)
   (signed-prekey-id :initarg :signed-prekey-id :accessor spkm-signed-prekey-id)
   (base-key :initarg :base-key :accessor spkm-base-key
             :documentation "Sender's ephemeral X25519 public key (33 bytes with 0x05 prefix)")
   (identity-key :initarg :identity-key :accessor spkm-identity-key
                 :documentation "Sender's identity public key (33 bytes with 0x05 prefix)")
   (whisper-message :initarg :whisper-message :accessor spkm-whisper-message
                    :documentation "Inner WhisperMessage (raw bytes)"))
  (:documentation "Parsed Signal Protocol PreKeyWhisperMessage."))

(defclass signal-whisper-message ()
  ((ratchet-key :initarg :ratchet-key :accessor swm-ratchet-key
                :documentation "Sender's ratchet public key (33 bytes with 0x05 prefix)")
   (counter :initarg :counter :accessor swm-counter)
   (previous-counter :initarg :previous-counter :accessor swm-previous-counter)
   (ciphertext :initarg :ciphertext :accessor swm-ciphertext
               :documentation "AES-CBC encrypted payload")
   (mac :initarg :mac :accessor swm-mac
        :documentation "8-byte HMAC-SHA256 truncated MAC")
   (serialized :initarg :serialized :accessor swm-serialized
               :documentation "Full serialized message (for MAC verification)"))
  (:documentation "Parsed Signal Protocol WhisperMessage."))

(defun strip-djb-prefix (key-bytes)
  "Remove 0x05 DJB type prefix from a public key if present.
   Returns 32-byte raw key."
  (if (and (= (length key-bytes) 33) (= (aref key-bytes 0) #x05))
      (subseq key-bytes 1)
      key-bytes))

(defun parse-prekey-whisper-message (data)
  "Parse a PreKeyWhisperMessage from raw bytes.
   Format: [version-byte(1)] [protobuf...]
   Protobuf fields: preKeyId(1), baseKey(2), identityKey(3), message(4), registrationId(5), signedPreKeyId(6)"
  (let* ((version-byte (aref data 0))
         (major (ash version-byte -4)))
    (unless (= major 3)
      (error "Unknown PreKeyWhisperMessage version: ~a" major))
    (let* ((protobuf-data (subseq data 1))
           (fields (protobuf-parse-fields protobuf-data))
           (prekey-id (or (cdr (assoc 1 fields)) 0))
           (base-key (cdr (assoc 2 fields)))
           (identity-key (cdr (assoc 3 fields)))
           (message (cdr (assoc 4 fields)))
           (registration-id (or (cdr (assoc 5 fields)) 0))
           (signed-prekey-id (or (cdr (assoc 6 fields)) 0)))
      (unless (and base-key identity-key message)
        (error "Incomplete PreKeyWhisperMessage"))
      (debug-log "Parsed PreKeyWhisperMessage: regId=~a preKeyId=~a signedPreKeyId=~a baseKey=~a bytes identityKey=~a bytes"
                 registration-id prekey-id signed-prekey-id (length base-key) (length identity-key))
      (make-instance 'signal-prekey-message
                     :registration-id registration-id
                     :prekey-id prekey-id
                     :signed-prekey-id signed-prekey-id
                     :base-key base-key
                     :identity-key identity-key
                     :whisper-message message))))

(defun parse-whisper-message (data)
  "Parse a WhisperMessage from raw bytes.
   Format: [version-byte(1)] [protobuf...] [mac(8)]
   Protobuf fields: ratchetKey(1), counter(2), previousCounter(3), ciphertext(4)"
  (let* ((version-byte (aref data 0))
         (major (ash version-byte -4))
         (mac-length 8))
    (unless (= major 3)
      (error "Unknown WhisperMessage version: ~a" major))
    (let* ((protobuf-data (subseq data 1 (- (length data) mac-length)))
           (mac (subseq data (- (length data) mac-length)))
           (fields (protobuf-parse-fields protobuf-data))
           (ratchet-key (cdr (assoc 1 fields)))
           (counter (or (cdr (assoc 2 fields)) 0))
           (previous-counter (or (cdr (assoc 3 fields)) 0))
           (ciphertext (cdr (assoc 4 fields))))
      (unless (and ratchet-key ciphertext)
        (error "Incomplete WhisperMessage"))
      (debug-log "Parsed WhisperMessage: counter=~a prevCounter=~a ratchetKey=~a bytes ciphertext=~a bytes"
                 counter previous-counter (length ratchet-key) (length ciphertext))
      (make-instance 'signal-whisper-message
                     :ratchet-key ratchet-key
                     :counter counter
                     :previous-counter previous-counter
                     :ciphertext ciphertext
                     :mac mac
                     :serialized data))))

;;; ============================================================
;;; Crypto Primitives
;;; ============================================================

(defun x25519-dh (private-key public-key)
  "Perform X25519 Diffie-Hellman. Returns shared secret (32 bytes)."
  (let ((priv (ironclad:make-private-key :curve25519 :x private-key :y (make-array 32 :element-type '(unsigned-byte 8))))
        (pub (ironclad:make-public-key :curve25519 :y public-key)))
    (ironclad:diffie-hellman priv pub)))

(defun concat-bytes (&rest byte-arrays)
  "Concatenate multiple byte arrays into one."
  (let* ((total-len (reduce #'+ byte-arrays :key #'length))
         (result (make-array total-len :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (arr byte-arrays result)
      (replace result arr :start1 pos)
      (incf pos (length arr)))))

(defun hmac-sha256 (key data)
  "Compute HMAC-SHA256 of DATA with KEY. Returns 32-byte digest."
  (let ((mac (ironclad:make-mac :hmac key :sha256)))
    (ironclad:update-mac mac data)
    (ironclad:produce-mac mac)))

(defun hmac-sha256-multi (key &rest data-parts)
  "Compute HMAC-SHA256 of multiple DATA-PARTS with KEY. Returns 32-byte digest."
  (let ((mac (ironclad:make-mac :hmac key :sha256)))
    (dolist (part data-parts)
      (when part (ironclad:update-mac mac part)))
    (ironclad:produce-mac mac)))

(defun hkdf-sha256 (input-key-material salt info length)
  "HKDF-SHA256 key derivation (RFC 5869).
   Returns LENGTH bytes derived from IKM using SALT and INFO.
   Implemented manually using HMAC-SHA256 since Ironclad lacks built-in HKDF."
  (let* ((hash-len 32)
         ;; Step 1: Extract - PRK = HMAC-Hash(salt, IKM)
         (effective-salt (if (and salt (> (length salt) 0))
                             salt
                             (make-array hash-len :element-type '(unsigned-byte 8) 
                                         :initial-element 0)))
         (prk (hmac-sha256 effective-salt input-key-material))
         ;; Step 2: Expand - generate LENGTH bytes
         (n (ceiling length hash-len))
         (okm (make-array (* n hash-len) :element-type '(unsigned-byte 8)))
         (t-prev (make-array 0 :element-type '(unsigned-byte 8))))
    (loop for i from 1 to n
          do (let* ((counter (make-array 1 :element-type '(unsigned-byte 8)
                                         :initial-element i))
                    (input (concat-bytes t-prev 
                                         (or info (make-array 0 :element-type '(unsigned-byte 8)))
                                         counter))
                    (t-current (hmac-sha256 prk input)))
               (replace okm t-current :start1 (* (1- i) hash-len))
               (setf t-prev t-current)))
    (subseq okm 0 length)))

(defun aes-cbc-decrypt (key iv ciphertext)
  "Decrypt CIPHERTEXT with AES-256-CBC using KEY and IV.
   Removes PKCS7 padding. Returns plaintext bytes."
  (let* ((cipher (ironclad:make-cipher :aes :mode :cbc :key key :initialization-vector iv))
         (plaintext (make-array (length ciphertext) :element-type '(unsigned-byte 8))))
    (ironclad:decrypt cipher ciphertext plaintext)
    ;; Remove PKCS7 padding
    (let ((pad-len (aref plaintext (1- (length plaintext)))))
      (if (and (> pad-len 0) (<= pad-len 16)
               (every (lambda (i) (= (aref plaintext i) pad-len))
                      (loop for i from (- (length plaintext) pad-len) below (length plaintext) collect i)))
          (subseq plaintext 0 (- (length plaintext) pad-len))
          plaintext))))

;;; ============================================================
;;; X3DH Key Agreement (Signal Protocol compatible)
;;; ============================================================
;;; 
;;; Signal Protocol X3DH as implemented in omemo_dr/libsignal:
;;; - Prepend 0xFF*32 discontinuity bytes
;;; - 4 DH agreements (identity keys are X25519, not Ed25519)
;;; - HKDF with salt=zeros(32), info="WhisperText" (v3) or "OMEMO Payload" (v4)
;;; - Output: 64 bytes -> root_key(32) + chain_key(32)

(defun x3dh-as-bob (their-identity-key-x25519 their-base-key
                    our-identity-key-seed our-signed-prekey-private our-one-time-prekey-private)
  "Perform X3DH as Bob (responder) using Signal Protocol conventions.
   THEIR-IDENTITY-KEY-X25519: sender's X25519 identity public key (32 bytes, already converted)
   THEIR-BASE-KEY: sender's ephemeral X25519 public key (32 bytes)
   OUR-IDENTITY-KEY-SEED: our Ed25519 private key seed (32 bytes)
   OUR-SIGNED-PREKEY-PRIVATE: our X25519 signed prekey private key (32 bytes)
   OUR-ONE-TIME-PREKEY-PRIVATE: our X25519 one-time prekey private key (32 bytes, or NIL)
   
   Returns (values root-key chain-key) each 32 bytes."
  (let* ((our-ik-x25519 (ed25519-private-to-x25519 our-identity-key-seed))
         ;; Signal Protocol X3DH as Bob:
         ;; DH1 = DH(our_signed_prekey, their_identity_key)
         (dh1 (x25519-dh our-signed-prekey-private their-identity-key-x25519))
         ;; DH2 = DH(our_identity_key, their_base_key)
         (dh2 (x25519-dh our-ik-x25519 their-base-key))
         ;; DH3 = DH(our_signed_prekey, their_base_key)
         (dh3 (x25519-dh our-signed-prekey-private their-base-key))
         ;; DH4 = DH(our_one_time_prekey, their_base_key) (if present)
         (dh4 (when our-one-time-prekey-private
                (x25519-dh our-one-time-prekey-private their-base-key)))
         ;; Prepend 0xFF*32 discontinuity bytes + concatenate DH outputs
         (discontinuity (make-array 32 :element-type '(unsigned-byte 8) :initial-element #xff))
         (secrets (if dh4
                      (concat-bytes discontinuity dh1 dh2 dh3 dh4)
                      (concat-bytes discontinuity dh1 dh2 dh3)))
         ;; Derive root key + chain key using HKDF
         (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (info (babel:string-to-octets "WhisperText"))
         (derived (hkdf-sha256 secrets salt info 64)))
    (debug-log "X3DH as Bob: dh1=~a dh2=~a dh3=~a dh4=~a"
               (ironclad:byte-array-to-hex-string (subseq dh1 0 4))
               (ironclad:byte-array-to-hex-string (subseq dh2 0 4))
               (ironclad:byte-array-to-hex-string (subseq dh3 0 4))
               (if dh4 (ironclad:byte-array-to-hex-string (subseq dh4 0 4)) "none"))
    (values (subseq derived 0 32) (subseq derived 32 64))))

;;; ============================================================
;;; Session Management
;;; ============================================================

(defclass omemo-session ()
  ((jid :initarg :jid :accessor session-jid)
   (device-id :initarg :device-id :accessor session-device-id)
   (root-key :initarg :root-key :accessor session-root-key
             :documentation "Current root key for Double Ratchet")
   (sending-chain-key :initarg :sending-chain-key :accessor session-sending-chain-key)
   (receiving-chain-key :initarg :receiving-chain-key :accessor session-receiving-chain-key)
   (sending-ratchet-key :initarg :sending-ratchet-key :accessor session-sending-ratchet-key
                        :documentation "Our current DH ratchet key pair (priv . pub)")
   (receiving-ratchet-key :initarg :receiving-ratchet-key :accessor session-receiving-ratchet-key
                          :documentation "Their current DH ratchet public key (32 bytes)")
   (our-identity-key-pub :initarg :our-identity-key-pub :accessor session-our-identity-key-pub
                         :documentation "Our X25519 identity public key (for MAC)")
   (their-identity-key-pub :initarg :their-identity-key-pub :accessor session-their-identity-key-pub
                           :documentation "Their X25519 identity public key (for MAC)")
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

;;; ============================================================
;;; Signal Protocol Double Ratchet (compatible with omemo_dr)
;;; ============================================================
;;;
;;; Chain key derivation (from omemo_dr/ratchet/chainkey.py):
;;;   message_key_material = HMAC-SHA256(chain_key, 0x01)
;;;   next_chain_key = HMAC-SHA256(chain_key, 0x02)
;;;   message_keys = HKDF(message_key_material, salt=zeros(32), info="WhisperMessageKeys", len=80)
;;;   -> cipher_key(32) + mac_key(32) + iv(16)
;;;
;;; Root key ratchet (from omemo_dr/ratchet/rootkey.py):
;;;   dh_output = DH(our_ratchet_priv, their_ratchet_pub)
;;;   derived = HKDF(dh_output, salt=root_key, info="WhisperRatchet", len=64)
;;;   -> new_root_key(32) + new_chain_key(32)

(defun signal-chain-key-derive-message-keys (chain-key)
  "Derive message keys from chain key using Signal Protocol conventions.
   Returns (values cipher-key mac-key iv) where cipher-key=32, mac-key=32, iv=16 bytes."
  (let* ((message-key-seed (make-array 1 :element-type '(unsigned-byte 8) :initial-element #x01))
         (message-key-material (hmac-sha256 chain-key message-key-seed))
         (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (info (babel:string-to-octets "WhisperMessageKeys"))
         (derived (hkdf-sha256 message-key-material salt info 80)))
    (values (subseq derived 0 32)    ; cipher_key (AES-256)
            (subseq derived 32 64)   ; mac_key (HMAC-SHA256)
            (subseq derived 64 80)))) ; iv (AES-CBC IV, 16 bytes)

(defun signal-chain-key-next (chain-key)
  "Derive next chain key from current chain key."
  (let ((chain-key-seed (make-array 1 :element-type '(unsigned-byte 8) :initial-element #x02)))
    (hmac-sha256 chain-key chain-key-seed)))

(defun signal-root-key-create-chain (root-key their-ratchet-pub our-ratchet-pair)
  "Perform DH ratchet step. Returns (values new-root-key new-chain-key)."
  (let* ((dh-output (x25519-dh (car our-ratchet-pair) their-ratchet-pub))
         (info (babel:string-to-octets "WhisperRatchet"))
         (derived (hkdf-sha256 dh-output root-key info 64)))
    (values (subseq derived 0 32)
            (subseq derived 32 64))))

(defun signal-verify-mac (mac-key sender-ik-serialized receiver-ik-serialized 
                          serialized-message expected-mac)
  "Verify the 8-byte truncated HMAC-SHA256 MAC on a WhisperMessage.
   sender/receiver IK are serialized (33 bytes with 0x05 prefix).
   serialized-message is version+protobuf (without MAC)."
  (let* ((message-without-mac (subseq serialized-message 0 (- (length serialized-message) 8)))
         (full-mac (hmac-sha256-multi mac-key 
                                      sender-ik-serialized
                                      receiver-ik-serialized
                                      message-without-mac))
         (truncated-mac (subseq full-mac 0 8)))
    (equalp truncated-mac expected-mac)))

(defun aes-gcm-encrypt (key iv plaintext)
  "Encrypt PLAINTEXT with AES-256-GCM using KEY and IV.
   Returns (values ciphertext tag) where tag is 16 bytes."
  (let* ((mode (ironclad:make-authenticated-encryption-mode 
                :gcm :cipher-name :aes :key key 
                :initialization-vector iv))
         (ct (ironclad:encrypt-message mode plaintext)))
    (let ((tag (ironclad:produce-tag mode)))
      (values ct tag))))

(defun aes-gcm-decrypt (key iv ciphertext tag)
  "Decrypt CIPHERTEXT with AES-256-GCM using KEY, IV, and TAG.
   Signals error if authentication fails.
   Returns plaintext bytes."
  (let ((mode (ironclad:make-authenticated-encryption-mode 
               :gcm :cipher-name :aes :key key 
               :initialization-vector iv :tag tag)))
    (ironclad:decrypt-message mode ciphertext)))

;;; ============================================================
;;; OMEMO Message Encryption/Decryption
;;; ============================================================

(defun decrypt-omemo-payload (msg-key iv payload)
  "Decrypt OMEMO payload using MSG-KEY and IV.
   PAYLOAD contains ciphertext + 16-byte GCM tag.
   Returns plaintext string."
  (let* ((tag-start (- (length payload) 16))
         (ct (subseq payload 0 tag-start))
         (tag (subseq payload tag-start))
         (plaintext (aes-gcm-decrypt msg-key iv ct tag)))
    (babel:octets-to-string plaintext :encoding :utf-8)))

(defun process-prekey-message (from-jid sender-device-id key-data)
  "Process a PreKey message: parse protobuf, perform X3DH, decrypt inner WhisperMessage.
   Returns the decrypted OMEMO message key bytes, or NIL on failure."
  (let* ((prekey-msg (parse-prekey-whisper-message key-data))
         ;; Extract keys (strip 0x05 DJB prefix)
         (their-ik-x25519 (strip-djb-prefix (spkm-identity-key prekey-msg)))
         (their-base-key (strip-djb-prefix (spkm-base-key prekey-msg)))
         (prekey-id (spkm-prekey-id prekey-msg))
         (signed-prekey-id (spkm-signed-prekey-id prekey-msg))
         ;; Our keys
         (our-ik (device-identity-key *omemo-device*))
         (our-spk (device-signed-prekey *omemo-device*))
         (our-opk (when (and prekey-id (> prekey-id 0))
                    (gethash prekey-id (device-prekeys *omemo-device*)))))
    (debug-log "PreKey msg: their-ik=~a their-base=~a preKeyId=~a signedPreKeyId=~a opk-found=~a our-spk-id=~a"
               (ironclad:byte-array-to-hex-string (subseq their-ik-x25519 0 4))
               (ironclad:byte-array-to-hex-string (subseq their-base-key 0 4))
               prekey-id signed-prekey-id (not (null our-opk))
               (signed-prekey-id our-spk))
    ;; Perform X3DH as Bob
    (multiple-value-bind (root-key chain-key)
        (x3dh-as-bob their-ik-x25519 their-base-key
                     (identity-private-key our-ik)
                     (signed-prekey-private our-spk)
                     (when our-opk (prekey-private our-opk)))
      ;; Our X25519 identity public key (for MAC verification)
      (let* ((our-ik-x25519-pub (ed25519-public-to-x25519 (identity-public-key our-ik)))
             ;; Create session with Bob's initial state:
             ;; - sender_chain = (our_signed_prekey, chain_key)
             ;; - root_key = root_key
             ;; Bob doesn't have a receiver chain yet - it gets created on first DH ratchet
             (session (make-instance 'omemo-session
                                     :jid from-jid
                                     :device-id sender-device-id
                                     :root-key root-key
                                     :sending-chain-key chain-key
                                     :receiving-chain-key nil
                                     :sending-ratchet-key (cons (signed-prekey-private our-spk)
                                                                (signed-prekey-public our-spk))
                                     :receiving-ratchet-key nil
                                     :our-identity-key-pub our-ik-x25519-pub
                                     :their-identity-key-pub their-ik-x25519)))
        (store-session session)
        ;; Remove used one-time prekey
        (when (and prekey-id (> prekey-id 0) our-opk)
          (remhash prekey-id (device-prekeys *omemo-device*))
          (debug-log "Consumed one-time prekey ~a, ~a remaining"
                     prekey-id (hash-table-count (device-prekeys *omemo-device*))))
        (debug-log "Created session as Bob with ~a device ~a" from-jid sender-device-id)
        ;; Now decrypt the inner WhisperMessage
        (decrypt-whisper-message session (spkm-whisper-message prekey-msg))))))

(defun decrypt-whisper-message (session whisper-data)
  "Decrypt a WhisperMessage using the session.
   WHISPER-DATA is the raw serialized WhisperMessage bytes.
   Returns decrypted plaintext bytes (the OMEMO message key), or NIL."
  (let* ((whisper-msg (parse-whisper-message whisper-data))
         (their-ratchet-key (strip-djb-prefix (swm-ratchet-key whisper-msg)))
         (counter (swm-counter whisper-msg))
         (ciphertext (swm-ciphertext whisper-msg)))
    (debug-log "Decrypting WhisperMessage: counter=~a ratchetKey=~a"
               counter (ironclad:byte-array-to-hex-string (subseq their-ratchet-key 0 4)))
    ;; Check if we need to perform DH ratchet step
    (when (or (null (session-receiving-ratchet-key session))
              (not (equalp their-ratchet-key (session-receiving-ratchet-key session))))
      (debug-log "Performing DH ratchet step")
      ;; Create receiver chain from DH ratchet
      (multiple-value-bind (new-root new-recv-chain)
          (signal-root-key-create-chain (session-root-key session)
                                        their-ratchet-key
                                        (session-sending-ratchet-key session))
        ;; Generate new sending ratchet key pair
        (multiple-value-bind (new-priv new-pub) (generate-x25519-keypair)
          (let ((new-sending-pair (cons new-priv new-pub)))
            (multiple-value-bind (new-root2 new-send-chain)
                (signal-root-key-create-chain new-root their-ratchet-key new-sending-pair)
              (setf (session-root-key session) new-root2)
              (setf (session-receiving-chain-key session) new-recv-chain)
              (setf (session-receiving-ratchet-key session) their-ratchet-key)
              (setf (session-sending-chain-key session) new-send-chain)
              (setf (session-sending-ratchet-key session) new-sending-pair))))))
    ;; Advance chain key to the right counter
    (let ((current-chain-key (session-receiving-chain-key session)))
      (unless current-chain-key
        (debug-log "No receiving chain key available")
        (return-from decrypt-whisper-message nil))
      ;; Skip chain keys up to counter (simplified - no out-of-order support yet)
      (loop for i from (session-recv-count session) below counter
            do (setf current-chain-key (signal-chain-key-next current-chain-key)))
      ;; Derive message keys
      (multiple-value-bind (cipher-key mac-key iv)
          (signal-chain-key-derive-message-keys current-chain-key)
        ;; Update session state
        (setf (session-receiving-chain-key session) (signal-chain-key-next current-chain-key))
        (setf (session-recv-count session) (1+ counter))
        ;; Verify MAC (optional for now, log but don't fail)
        (let* ((our-ik-serialized (prepend-djb-type (session-our-identity-key-pub session)))
               (their-ik-serialized (prepend-djb-type (session-their-identity-key-pub session)))
               (mac-valid (signal-verify-mac mac-key their-ik-serialized our-ik-serialized
                                             whisper-data (swm-mac whisper-msg))))
          (debug-log "WhisperMessage MAC verification: ~a" (if mac-valid "PASS" "FAIL"))
          (unless mac-valid
            (debug-log "WARNING: MAC verification failed, attempting decrypt anyway")))
        ;; Decrypt with AES-CBC
        (debug-log "Decrypting with AES-CBC: cipher-key=~a iv=~a ciphertext=~a bytes"
                   (ironclad:byte-array-to-hex-string (subseq cipher-key 0 4))
                   (ironclad:byte-array-to-hex-string iv)
                   (length ciphertext))
        (aes-cbc-decrypt cipher-key iv ciphertext)))))

(defun omemo-decrypt-message (encrypted-el from-jid)
  "Decrypt OMEMO encrypted message element.
   Handles both PreKey messages (new sessions) and regular messages.
   Returns plaintext string or NIL if decryption fails."
  (unless *omemo-device*
    (debug-log "OMEMO not initialized, cannot decrypt")
    (return-from omemo-decrypt-message nil))
  (handler-case
      (let ((omemo-msg (parse-omemo-encrypted encrypted-el)))
        (unless omemo-msg
          (debug-log "Failed to parse OMEMO encrypted element")
          (return-from omemo-decrypt-message nil))
        (let* ((sender-device-id (omemo-sender-device-id omemo-msg))
               (my-device-id (device-id *omemo-device*))
               (my-key (find-key-for-device omemo-msg my-device-id)))
          (debug-log "OMEMO msg from device ~a, keys for devices: ~a, looking for our device: ~a"
                     sender-device-id
                     (mapcar #'key-recipient-device-id (omemo-keys omemo-msg))
                     my-device-id)
          (unless my-key
            (debug-log "No OMEMO key element for our device ~a" my-device-id)
            (return-from omemo-decrypt-message nil))
          ;; The key-data is a serialized Signal Protocol message
          (let* ((key-data (key-data my-key))
                 (is-prekey (key-is-prekey my-key))
                 ;; Decrypt the key element to get the OMEMO message key
                 (omemo-msg-key
                   (if is-prekey
                       ;; PreKey message: parse protobuf, X3DH, decrypt WhisperMessage
                       (progn
                         (debug-log "Processing PreKey message from ~a device ~a"
                                    from-jid sender-device-id)
                         (process-prekey-message from-jid sender-device-id key-data))
                       ;; Regular message: decrypt with existing session
                       (let ((session (get-session from-jid sender-device-id)))
                         (if session
                             (decrypt-whisper-message session key-data)
                             (progn
                               (debug-log "No session for ~a device ~a, not a PreKey message"
                                          from-jid sender-device-id)
                               nil))))))
            (debug-log "After Signal decrypt: omemo-msg-key=~a (~a) payload=~a"
                       (if omemo-msg-key 
                           (format nil "~a bytes: ~a" (length omemo-msg-key)
                                   (ironclad:byte-array-to-hex-string 
                                    (subseq omemo-msg-key 0 (min 8 (length omemo-msg-key)))))
                           "NIL")
                       (type-of omemo-msg-key)
                       (if (omemo-payload omemo-msg) 
                           (format nil "~a bytes" (length (omemo-payload omemo-msg)))
                           "NIL"))
            (when (and omemo-msg-key (omemo-payload omemo-msg))
              ;; omemo_dr aes_decrypt logic:
              ;;   if len(key) >= 32: XEP-compliant
              ;;     aes_key = key[:16]   (AES-128!)
              ;;     tag = key[16:]       (GCM auth tag)
              ;;     data = payload       (pure ciphertext, no tag appended)
              ;;   else: legacy
              ;;     aes_key = key
              ;;     data = payload[:-16]
              ;;     tag = payload[-16:]
              (let* ((key-len (length omemo-msg-key))
                     (iv (omemo-iv omemo-msg))
                     (payload (omemo-payload omemo-msg)))
                (multiple-value-bind (aes-key tag ct)
                    (if (>= key-len 32)
                        ;; XEP-compliant: key(16) + tag(16), payload = pure ciphertext
                        (values (subseq omemo-msg-key 0 16)
                                (subseq omemo-msg-key 16 32)
                                payload)
                        ;; Legacy: key is the AES key, tag is last 16 bytes of payload
                        (values omemo-msg-key
                                (subseq payload (- (length payload) 16))
                                (subseq payload 0 (- (length payload) 16))))
                  (debug-log "Decrypting OMEMO payload: aes-key=~a(~a) iv=~a ct=~a tag=~a"
                             (ironclad:byte-array-to-hex-string aes-key) (length aes-key)
                             (length iv) (length ct) (length tag))
                  (let ((plaintext (aes-gcm-decrypt aes-key iv ct tag)))
                    (babel:octets-to-string plaintext :encoding :utf-8))))))))
    (error (e)
      (debug-log "OMEMO decryption error: ~a (~a)" e (type-of e))
      nil)))

(defun omemo-enabled-for-jid-p (jid)
  "Check if we have OMEMO sessions established for JID."
  (and *omemo-device*
       (get-cached-device-list jid)
       (some (lambda (did) (get-session jid did))
             (get-cached-device-list jid))))

(defun omemo-encrypt-message (jid message)
  "Encrypt MESSAGE for all devices of JID.
   Returns OMEMO XML element for the encrypted message, or NIL if no sessions."
  (unless *omemo-device*
    (debug-log "OMEMO not initialized, cannot encrypt")
    (return-from omemo-encrypt-message nil))
  (let* ((device-ids (get-cached-device-list jid))
         (key-elements nil)
         (msg-key (generate-random-bytes 32))
         (iv (generate-random-bytes 12))
         (plaintext (babel:string-to-octets message :encoding :utf-8)))
    (multiple-value-bind (ciphertext tag) (aes-gcm-encrypt msg-key iv plaintext)
      (let ((payload (concat-bytes ciphertext tag)))
        ;; For each recipient device, encrypt the message key with their session
        ;; TODO: implement Signal Protocol encryption (currently placeholder)
        (dolist (device-id device-ids)
          (let ((session (get-session jid device-id)))
            (when session
              ;; Placeholder - needs proper Signal Protocol WhisperMessage construction
              (push (make-xml-element "key"
                                      :attributes `(("rid" . ,(princ-to-string device-id)))
                                      :text (bytes-to-base64 msg-key))
                    key-elements))))
        ;; Build OMEMO encrypted element (use legacy namespace for max compatibility)
        (when key-elements
          (make-xml-element "encrypted"
                            :namespace +omemo-legacy-ns+
                            :children (list
                                       (make-xml-element "header"
                                                         :attributes `(("sid" . ,(princ-to-string (device-id *omemo-device*))))
                                                         :children (append
                                                                    key-elements
                                                                    (list (make-xml-element "iv"
                                                                                            :text (bytes-to-base64 iv)))))
                                       (make-xml-element "payload"
                                                         :text (bytes-to-base64 payload)))))))))

(defun create-session-as-initiator (jid device-id bundle)
  "Create new OMEMO session as initiator using remote BUNDLE.
   Returns the new session."
  ;; TODO: implement proper Signal Protocol session initiation
  ;; For now, just log that we would create a session
  (debug-log "Would create OMEMO session with ~a device ~a (not yet implemented)" jid device-id)
  nil)
