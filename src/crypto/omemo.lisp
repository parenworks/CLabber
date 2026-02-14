(in-package #:clabber.crypto)

;;; OMEMO XEP-0384 Stanza Layer
;;; Handles OMEMO XML stanza construction/parsing and AES-128-GCM payload encryption.
;;; Signal Protocol operations (X3DH, Double Ratchet, session management) are
;;; delegated to libsignal-protocol-c via signal-ffi.lisp.

(defvar *omemo-device-id* nil "Our OMEMO device ID")
(defvar *omemo-registration-id* nil "Our Signal registration ID")

;;; OMEMO namespaces
(alexandria:define-constant +omemo-ns-legacy+ "eu.siacs.conversations.axolotl"
  :test #'string= :documentation "Legacy OMEMO namespace (used by Gajim, Conversations, etc.)")
(alexandria:define-constant +omemo-ns-modern+ "urn:xmpp:omemo:2"
  :test #'string= :documentation "Modern OMEMO namespace (XEP-0384 v0.8+)")
(alexandria:define-constant +omemo-devicelist-node-legacy+ "eu.siacs.conversations.axolotl.devicelist"
  :test #'string=)
(alexandria:define-constant +omemo-bundles-node-legacy+ "eu.siacs.conversations.axolotl.bundles:"
  :test #'string=)

;;; ============================================================
;;; Utility
;;; ============================================================

(defun ensure-usb8 (bytes)
  "Ensure BYTES is a (simple-array (unsigned-byte 8) (*)).
   CFFI foreign-array-to-lisp may return arrays with wrong element type."
  (if (typep bytes '(simple-array (unsigned-byte 8) (*)))
      bytes
      (let ((result (make-array (length bytes) :element-type '(unsigned-byte 8))))
        (dotimes (i (length bytes) result)
          (setf (aref result i) (aref bytes i))))))

(defun bytes-to-base64 (bytes)
  "Encode byte array to base64 string."
  (cl-base64:usb8-array-to-base64-string (ensure-usb8 bytes)))

(defun base64-to-bytes (string)
  "Decode base64 string to byte array."
  (cl-base64:base64-string-to-usb8-array string))

;;; Device list cache: JID -> list of device IDs
(defvar *device-list-cache* (make-hash-table :test 'equal))

(defun cache-device-list (jid device-ids)
  "Cache/merge device IDs for a JID."
  (let ((existing (gethash jid *device-list-cache*)))
    (setf (gethash jid *device-list-cache*)
          (remove-duplicates (append existing device-ids)))))

(defun get-cached-device-list (jid)
  (gethash jid *device-list-cache*))

(defun omemo-enabled-p (jid)
  "Check if OMEMO is available for JID (we have cached device IDs)."
  (not (null (get-cached-device-list jid))))

;;; ============================================================
;;; OMEMO initialization
;;; ============================================================

(defun omemo-init ()
  "Initialize OMEMO: load or generate identity, prekeys, signed prekey."
  (signal-init)
  (unless (signal-has-identity-p)
    ;; Generate new identity (saves to disk)
    (signal-generate-identity))
  (let ((reg-id (signal-get-registration-id)))
    (setf *omemo-registration-id* reg-id)
    (setf *omemo-device-id* reg-id))
  ;; Always (re)generate prekeys and signed prekey so they're available
  (signal-generate-prekeys 1 100)
  (signal-generate-signed-prekey 1)
  (values *omemo-device-id* (signal-get-identity-public)))

;;; ============================================================
;;; Bundle publishing (XML construction)
;;; ============================================================

(defun omemo-bundle-xml ()
  "Generate the OMEMO bundle XML for PEP publishing.
   Returns XML string for the legacy namespace bundle."
  (let* ((identity-pub (signal-get-identity-public))
         (prekeys (signal-generate-prekeys 1 100)))
    (multiple-value-bind (spk-pub spk-sig)
        (signal-generate-signed-prekey 1)
      (with-output-to-string (s)
        (format s "<bundle xmlns='~A'>" +omemo-ns-legacy+)
        (format s "<signedPreKeyPublic signedPreKeyId='1'>~A</signedPreKeyPublic>"
                (bytes-to-base64 spk-pub))
        (format s "<signedPreKeySignature>~A</signedPreKeySignature>"
                (bytes-to-base64 spk-sig))
        (format s "<identityKey>~A</identityKey>"
                (bytes-to-base64 identity-pub))
        (format s "<prekeys>")
        (dolist (pk prekeys)
          (format s "<preKeyPublic preKeyId='~A'>~A</preKeyPublic>"
                  (car pk) (bytes-to-base64 (cdr pk))))
        (format s "</prekeys>")
        (format s "</bundle>")))))

;;; ============================================================
;;; AES-128-GCM payload encryption/decryption
;;; (This is the OMEMO payload layer, NOT the Signal session layer)
;;; ============================================================

(defun omemo-encrypt-payload (plaintext-string)
  "Encrypt a message payload with AES-128-GCM.
   Returns (values iv ciphertext key+tag) where key+tag is 32 bytes
   (16-byte key + 16-byte GCM authentication tag)."
  ;; Generate random 16-byte key and 12-byte IV
  (let* ((key (ironclad:random-data 16))
         (iv (ironclad:random-data 12))
         (plaintext (babel:string-to-octets plaintext-string :encoding :utf-8))
         ;; Encrypt with AES-128-GCM
         (cipher (ironclad:make-authenticated-encryption-mode
                  :gcm :cipher-name :aes :key key
                  :initialization-vector iv))
         (ciphertext (make-array (length plaintext) :element-type '(unsigned-byte 8))))
    (ironclad:process-associated-data cipher (make-array 0 :element-type '(unsigned-byte 8)))
    (ironclad:encrypt cipher plaintext ciphertext)
    (let ((tag (ironclad:produce-tag cipher)))
      ;; key-material = key(16) || tag(16) = 32 bytes
      ;; This is what gets encrypted per-device with Signal session
      (let ((key-material (make-array 32 :element-type '(unsigned-byte 8))))
        (replace key-material key :start1 0 :end1 16)
        (replace key-material tag :start1 16 :end1 32)
        (values iv ciphertext key-material)))))

(defun omemo-decrypt-payload (iv ciphertext key-material)
  "Decrypt an OMEMO payload with AES-128-GCM.
   KEY-MATERIAL is 32 bytes: key(16) + tag(16)."
  (let* ((km (ensure-usb8 key-material))
         (iv8 (ensure-usb8 iv))
         (ct8 (ensure-usb8 ciphertext))
         (key (subseq km 0 16))
         (tag (subseq km 16 32))
         (cipher (ironclad:make-authenticated-encryption-mode
                  :gcm :cipher-name :aes :key key
                  :initialization-vector iv8))
         (plaintext (make-array (length ct8) :element-type '(unsigned-byte 8))))
    (ironclad:process-associated-data cipher (make-array 0 :element-type '(unsigned-byte 8)))
    (ironclad:decrypt cipher ct8 plaintext)
    ;; Verify tag
    (unless (equalp tag (ironclad:produce-tag cipher))
      (error "Bad authentication tag"))
    (babel:octets-to-string plaintext :encoding :utf-8)))

;;; ============================================================
;;; OMEMO message encryption (full flow)
;;; ============================================================

(defun omemo-encrypt-message (to-jid plaintext &optional our-jid)
  "Encrypt a message for TO-JID using OMEMO.
   Also encrypts for our own devices so other clients can read sent messages.
   OUR-JID is our bare JID for looking up own device list.
   Returns XML string for the OMEMO message stanza body."
  (let ((device-ids (get-cached-device-list to-jid)))
    (unless device-ids
      (error "No OMEMO devices known for ~A" to-jid))
    (multiple-value-bind (iv ciphertext key-material)
        (omemo-encrypt-payload plaintext)
      (with-output-to-string (s)
        (format s "<encrypted xmlns='~A'>" +omemo-ns-legacy+)
        (format s "<header sid='~A'>" *omemo-device-id*)
        ;; Encrypt key-material for each recipient device
        (dolist (device-id device-ids)
          (handler-case
              (multiple-value-bind (encrypted-key msg-type)
                  (signal-session-encrypt to-jid device-id key-material)
                (format s "<key rid='~A'~A>~A</key>"
                        device-id
                        (if (= msg-type +ciphertext-prekey-type+) " prekey='true'" "")
                        (bytes-to-base64 encrypted-key)))
            (error (e)
              (debug-log "OMEMO: can't encrypt to ~A/~A: ~A" to-jid device-id e))))
        ;; Also encrypt for our own other devices
        (when our-jid
          (let ((own-devices (get-cached-device-list our-jid)))
            (dolist (device-id own-devices)
              (unless (= device-id *omemo-device-id*)  ; skip ourselves
                (handler-case
                    (multiple-value-bind (encrypted-key msg-type)
                        (signal-session-encrypt our-jid device-id key-material)
                      (format s "<key rid='~A'~A>~A</key>"
                              device-id
                              (if (= msg-type +ciphertext-prekey-type+) " prekey='true'" "")
                              (bytes-to-base64 encrypted-key)))
                  (error (e)
                    (debug-log "OMEMO: can't encrypt to own ~A/~A: ~A" our-jid device-id e)))))))
        (format s "<iv>~A</iv>" (bytes-to-base64 iv))
        (format s "</header>")
        (format s "<payload>~A</payload>" (bytes-to-base64 ciphertext))
        (format s "</encrypted>")))))

(defun omemo-decrypt-message (from-jid sender-device-id key-data is-prekey iv ciphertext)
  "Decrypt an incoming OMEMO message.
   KEY-DATA is the base64-decoded <key> element for our device.
   Returns the plaintext string."
  ;; Cache sender's device ID
  (cache-device-list from-jid (list sender-device-id))
  ;; Decrypt the key material using Signal session
  (let ((key-material (ensure-usb8
                       (signal-session-decrypt from-jid sender-device-id
                                               (ensure-usb8 key-data)
                                               :prekey-p is-prekey))))
    ;; Decrypt the payload
    (omemo-decrypt-payload iv ciphertext key-material)))

