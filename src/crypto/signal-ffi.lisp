(in-package #:clabber.crypto)

;;; CFFI Bindings to libsignal-protocol-c
;;; Provides Signal Protocol operations: key generation, session management,
;;; encrypt/decrypt via the battle-tested C library.
;;;
;;; Architecture: We use a thin C helper library (clabber-signal-helper.so) that
;;; wraps libsignal-protocol-c with OpenSSL-based crypto callbacks and
;;; file-based store callbacks. This avoids the complexity of implementing
;;; C callback functions from CFFI.

;;; Helper: allocate foreign memory from a Lisp byte vector, execute body, free.
(defmacro with-foreign-byte-array ((var lisp-array) &body body)
  "Allocate foreign memory, copy LISP-ARRAY into it, bind to VAR, free on exit."
  (let ((len-sym (gensym "LEN")))
    `(let* ((,len-sym (length ,lisp-array))
            (,var (cffi:foreign-alloc :uint8 :count ,len-sym)))
       (unwind-protect
            (progn
              (loop for i below ,len-sym
                    do (setf (cffi:mem-aref ,var :uint8 i) (aref ,lisp-array i)))
              ,@body)
         (cffi:foreign-free ,var)))))

(cffi:define-foreign-library libsignal-protocol-c
  (:unix "libsignal-protocol-c.so")
  (t (:default "libsignal-protocol-c")))

(cffi:define-foreign-library libclabber-signal
  (:unix "libclabber-signal.so")
  (t (:default "libclabber-signal")))

;;; ============================================================
;;; Error codes from signal_protocol.h
;;; ============================================================

(defconstant +sg-success+ 0)
(defconstant +sg-err-nomem+ -12)
(defconstant +sg-err-inval+ -22)
(defconstant +sg-err-unknown+ -1000)
(defconstant +sg-err-duplicate-message+ -1001)
(defconstant +sg-err-invalid-key+ -1002)
(defconstant +sg-err-invalid-key-id+ -1003)
(defconstant +sg-err-invalid-mac+ -1004)
(defconstant +sg-err-invalid-message+ -1005)
(defconstant +sg-err-invalid-version+ -1006)
(defconstant +sg-err-legacy-message+ -1007)
(defconstant +sg-err-no-session+ -1008)
(defconstant +sg-err-untrusted-identity+ -1010)

;;; Ciphertext types
(defconstant +ciphertext-signal-type+ 2)
(defconstant +ciphertext-prekey-type+ 3)

;;; ============================================================
;;; Direct libsignal-protocol-c bindings (low-level)
;;; Used by the helper library, but also available for direct use
;;; ============================================================

;;; Buffer operations
(cffi:defcfun "signal_buffer_alloc" :pointer (len :size))
(cffi:defcfun "signal_buffer_create" :pointer (data :pointer) (len :size))
(cffi:defcfun "signal_buffer_data" :pointer (buffer :pointer))
(cffi:defcfun "signal_buffer_len" :size (buffer :pointer))
(cffi:defcfun "signal_buffer_free" :void (buffer :pointer))
(cffi:defcfun "signal_buffer_bzero_free" :void (buffer :pointer))

;;; Type ref counting
(cffi:defcfun "signal_type_ref" :void (instance :pointer))
(cffi:defcfun "signal_type_unref" :void (instance :pointer))

;;; Context
(cffi:defcfun "signal_context_create" :int (context :pointer) (user-data :pointer))
(cffi:defcfun "signal_context_set_crypto_provider" :int (context :pointer) (provider :pointer))
(cffi:defcfun "signal_context_set_locking_functions" :int
  (context :pointer) (lock :pointer) (unlock :pointer))
(cffi:defcfun "signal_context_set_log_function" :int (context :pointer) (log-fn :pointer))
(cffi:defcfun "signal_context_destroy" :void (context :pointer))

;;; Store context
(cffi:defcfun "signal_protocol_store_context_create" :int (context :pointer) (global-context :pointer))
(cffi:defcfun "signal_protocol_store_context_set_session_store" :int (context :pointer) (store :pointer))
(cffi:defcfun "signal_protocol_store_context_set_pre_key_store" :int (context :pointer) (store :pointer))
(cffi:defcfun "signal_protocol_store_context_set_signed_pre_key_store" :int (context :pointer) (store :pointer))
(cffi:defcfun "signal_protocol_store_context_set_identity_key_store" :int (context :pointer) (store :pointer))
(cffi:defcfun "signal_protocol_store_context_destroy" :void (context :pointer))

;;; Key helpers
(cffi:defcfun "signal_protocol_key_helper_generate_identity_key_pair" :int
  (key-pair :pointer) (global-context :pointer))
(cffi:defcfun "signal_protocol_key_helper_generate_registration_id" :int
  (registration-id :pointer) (extended-range :int) (global-context :pointer))
(cffi:defcfun "signal_protocol_key_helper_generate_pre_keys" :int
  (head :pointer) (start :unsigned-int) (count :unsigned-int) (global-context :pointer))
(cffi:defcfun "signal_protocol_key_helper_generate_signed_pre_key" :int
  (signed-pre-key :pointer) (identity-key-pair :pointer)
  (signed-pre-key-id :uint32) (timestamp :uint64) (global-context :pointer))
(cffi:defcfun "signal_protocol_key_helper_key_list_element" :pointer (node :pointer))
(cffi:defcfun "signal_protocol_key_helper_key_list_next" :pointer (node :pointer))
(cffi:defcfun "signal_protocol_key_helper_key_list_free" :void (head :pointer))

;;; Pre key operations
(cffi:defcfun "session_pre_key_get_id" :uint32 (pre-key :pointer))
(cffi:defcfun "session_pre_key_get_key_pair" :pointer (pre-key :pointer))
(cffi:defcfun "session_pre_key_serialize" :int (buffer :pointer) (pre-key :pointer))
(cffi:defcfun "session_pre_key_deserialize" :int
  (pre-key :pointer) (data :pointer) (len :size) (global-context :pointer))

;;; Signed pre key operations
(cffi:defcfun "session_signed_pre_key_get_id" :uint32 (pre-key :pointer))
(cffi:defcfun "session_signed_pre_key_get_key_pair" :pointer (pre-key :pointer))
(cffi:defcfun "session_signed_pre_key_get_signature" :pointer (pre-key :pointer))
(cffi:defcfun "session_signed_pre_key_get_signature_len" :size (pre-key :pointer))
(cffi:defcfun "session_signed_pre_key_serialize" :int (buffer :pointer) (pre-key :pointer))
(cffi:defcfun "session_signed_pre_key_deserialize" :int
  (pre-key :pointer) (data :pointer) (len :size) (global-context :pointer))

;;; Curve operations
(cffi:defcfun "curve_decode_point" :int
  (public-key :pointer) (key-data :pointer) (key-len :size) (global-context :pointer))
(cffi:defcfun "curve_decode_private_point" :int
  (private-key :pointer) (key-data :pointer) (key-len :size) (global-context :pointer))
(cffi:defcfun "ec_public_key_serialize" :int (buffer :pointer) (key :pointer))
(cffi:defcfun "ec_private_key_serialize" :int (buffer :pointer) (key :pointer))
(cffi:defcfun "ec_key_pair_get_public" :pointer (key-pair :pointer))
(cffi:defcfun "ec_key_pair_get_private" :pointer (key-pair :pointer))

;;; Identity key pair
(cffi:defcfun "ratchet_identity_key_pair_get_public" :pointer (key-pair :pointer))
(cffi:defcfun "ratchet_identity_key_pair_get_private" :pointer (key-pair :pointer))

;;; Session builder
(cffi:defcfun "session_builder_create" :int
  (builder :pointer) (store :pointer) (remote-address :pointer) (global-context :pointer))
(cffi:defcfun "session_builder_process_pre_key_bundle" :int
  (builder :pointer) (bundle :pointer))
(cffi:defcfun "session_builder_free" :void (builder :pointer))

;;; Pre key bundle
(cffi:defcfun "session_pre_key_bundle_create" :int
  (bundle :pointer) (registration-id :uint32) (device-id :int)
  (pre-key-id :uint32) (pre-key-public :pointer)
  (signed-pre-key-id :uint32) (signed-pre-key-public :pointer)
  (signed-pre-key-signature :pointer) (signed-pre-key-signature-len :size)
  (identity-key :pointer))

;;; Session cipher
(cffi:defcfun "session_cipher_create" :int
  (cipher :pointer) (store :pointer) (remote-address :pointer) (global-context :pointer))
(cffi:defcfun "session_cipher_encrypt" :int
  (cipher :pointer) (padded-message :pointer) (padded-message-len :size)
  (encrypted-message :pointer))
(cffi:defcfun "session_cipher_decrypt_pre_key_signal_message" :int
  (cipher :pointer) (ciphertext :pointer) (decrypt-context :pointer) (plaintext :pointer))
(cffi:defcfun "session_cipher_decrypt_signal_message" :int
  (cipher :pointer) (ciphertext :pointer) (decrypt-context :pointer) (plaintext :pointer))
(cffi:defcfun "session_cipher_free" :void (cipher :pointer))

;;; Ciphertext message
(cffi:defcfun "ciphertext_message_get_type" :int (message :pointer))
(cffi:defcfun "ciphertext_message_get_serialized" :pointer (message :pointer))

;;; Signal/PreKey message deserialization
(cffi:defcfun "signal_message_deserialize" :int
  (message :pointer) (data :pointer) (len :size) (global-context :pointer))
(cffi:defcfun "pre_key_signal_message_deserialize" :int
  (message :pointer) (data :pointer) (len :size) (global-context :pointer))

;;; Session containment check
(cffi:defcfun "signal_protocol_session_contains_session" :int
  (context :pointer) (address :pointer))

;;; ============================================================
;;; Helper library bindings (high-level C wrapper)
;;; ============================================================

;;; The helper library provides:
;;; - OpenSSL-based crypto provider setup
;;; - File-based session/prekey/identity stores
;;; - Simplified init/cleanup API

(cffi:defcfun ("clabber_signal_init" %signal-helper-init) :int
  (store-path :string))

(cffi:defcfun ("clabber_signal_cleanup" %signal-helper-cleanup) :void)

(cffi:defcfun ("clabber_signal_get_context" %signal-get-context) :pointer)

(cffi:defcfun ("clabber_signal_get_store" %signal-get-store) :pointer)

(cffi:defcfun ("clabber_signal_generate_identity" %signal-generate-identity) :int
  (registration-id-out :pointer)
  (identity-pub-out :pointer) (identity-pub-len-out :pointer)
  (identity-priv-out :pointer) (identity-priv-len-out :pointer))

(cffi:defcfun ("clabber_signal_generate_prekeys" %signal-generate-prekeys) :int
  (start :unsigned-int) (count :unsigned-int))

(cffi:defcfun ("clabber_signal_generate_signed_prekey" %signal-generate-signed-prekey) :int
  (signed-prekey-id :uint32))

(cffi:defcfun ("clabber_signal_get_prekey_public" %signal-get-prekey-public) :int
  (prekey-id :uint32) (pub-out :pointer) (pub-len-out :pointer))

(cffi:defcfun ("clabber_signal_get_signed_prekey_public" %signal-get-signed-prekey-public) :int
  (signed-prekey-id :uint32)
  (pub-out :pointer) (pub-len-out :pointer)
  (sig-out :pointer) (sig-len-out :pointer))

(cffi:defcfun ("clabber_signal_get_identity_public" %signal-get-identity-public) :int
  (pub-out :pointer) (pub-len-out :pointer))

(cffi:defcfun ("clabber_signal_build_session" %signal-build-session) :int
  (name :string) (device-id :int32)
  (registration-id :uint32)
  (prekey-id :uint32) (prekey-public :pointer) (prekey-public-len :size)
  (signed-prekey-id :uint32) (signed-prekey-public :pointer) (signed-prekey-public-len :size)
  (signed-prekey-signature :pointer) (signed-prekey-signature-len :size)
  (identity-key :pointer) (identity-key-len :size))

(cffi:defcfun ("clabber_signal_encrypt" %signal-encrypt) :int
  (name :string) (device-id :int32)
  (plaintext :pointer) (plaintext-len :size)
  (ciphertext-out :pointer) (ciphertext-len-out :pointer)
  (msg-type-out :pointer))

(cffi:defcfun ("clabber_signal_decrypt" %signal-decrypt) :int
  (name :string) (device-id :int32)
  (ciphertext :pointer) (ciphertext-len :size)
  (is-prekey :int)
  (plaintext-out :pointer) (plaintext-len-out :pointer))

(cffi:defcfun ("clabber_signal_has_session" %signal-has-session) :int
  (name :string) (device-id :int32))

;;; ============================================================
;;; Lisp-level API
;;; ============================================================

(defvar *signal-initialized* nil)

(defun signal-init (&optional (store-path nil))
  "Initialize the Signal Protocol library with file-based stores.
   STORE-PATH defaults to ~/.config/clabber/signal/"
  (unless *signal-initialized*
    (cffi:use-foreign-library libsignal-protocol-c)
    (cffi:use-foreign-library libclabber-signal)
    (let ((path (or store-path
                    (namestring (merge-pathnames "signal/"
                                                 (merge-pathnames ".config/clabber/"
                                                                  (user-homedir-pathname)))))))
      (ensure-directories-exist path)
      (let ((result (%signal-helper-init path)))
        (unless (zerop result)
          (error "Failed to initialize Signal Protocol library: ~A" result))
        (setf *signal-initialized* t)))))

(defun signal-cleanup ()
  "Clean up Signal Protocol library resources."
  (when *signal-initialized*
    (%signal-helper-cleanup)
    (setf *signal-initialized* nil)))

(defun signal-generate-identity ()
  "Generate a new identity key pair and registration ID.
   Returns (values registration-id identity-pub-bytes identity-priv-bytes)"
  (cffi:with-foreign-objects ((reg-id :uint32)
                              (pub-ptr :pointer)
                              (pub-len :size)
                              (priv-ptr :pointer)
                              (priv-len :size))
    (let ((result (%signal-generate-identity reg-id pub-ptr pub-len priv-ptr priv-len)))
      (unless (zerop result)
        (error "Failed to generate identity: ~A" result))
      (let* ((pub-size (cffi:mem-ref pub-len :size))
             (priv-size (cffi:mem-ref priv-len :size))
             (pub-data (cffi:mem-ref pub-ptr :pointer))
             (priv-data (cffi:mem-ref priv-ptr :pointer))
             (pub-bytes (cffi:foreign-array-to-lisp pub-data `(:array :uint8 ,pub-size)))
             (priv-bytes (cffi:foreign-array-to-lisp priv-data `(:array :uint8 ,priv-size))))
        (cffi:foreign-free pub-data)
        (cffi:foreign-free priv-data)
        (values (cffi:mem-ref reg-id :uint32) pub-bytes priv-bytes)))))

(defun signal-generate-prekeys (start count)
  "Generate COUNT pre-keys starting from START ID.
   Returns list of (id . public-key-bytes) pairs."
  (let ((result (%signal-generate-prekeys start count)))
    (unless (zerop result)
      (error "Failed to generate pre-keys: ~A" result))
    ;; Retrieve each pre-key's public key
    (loop for id from start below (+ start count)
          collect (cffi:with-foreign-objects ((pub-ptr :pointer) (pub-len :size))
                    (let ((r (%signal-get-prekey-public id pub-ptr pub-len)))
                      (unless (zerop r)
                        (error "Failed to get pre-key ~A public: ~A" id r))
                      (let* ((size (cffi:mem-ref pub-len :size))
                             (data (cffi:mem-ref pub-ptr :pointer))
                             (bytes (cffi:foreign-array-to-lisp data `(:array :uint8 ,size))))
                        (cffi:foreign-free data)
                        (cons id bytes)))))))

(defun signal-generate-signed-prekey (signed-prekey-id)
  "Generate a signed pre-key.
   Returns (values public-key-bytes signature-bytes)"
  (let ((result (%signal-generate-signed-prekey signed-prekey-id)))
    (unless (zerop result)
      (error "Failed to generate signed pre-key: ~A" result))
    (cffi:with-foreign-objects ((pub-ptr :pointer) (pub-len :size)
                                (sig-ptr :pointer) (sig-len :size))
      (let ((r (%signal-get-signed-prekey-public signed-prekey-id pub-ptr pub-len sig-ptr sig-len)))
        (unless (zerop r)
          (error "Failed to get signed pre-key public: ~A" r))
        (let* ((pub-size (cffi:mem-ref pub-len :size))
               (sig-size (cffi:mem-ref sig-len :size))
               (pub-data (cffi:mem-ref pub-ptr :pointer))
               (sig-data (cffi:mem-ref sig-ptr :pointer))
               (pub-bytes (cffi:foreign-array-to-lisp pub-data `(:array :uint8 ,pub-size)))
               (sig-bytes (cffi:foreign-array-to-lisp sig-data `(:array :uint8 ,sig-size))))
          (cffi:foreign-free pub-data)
          (cffi:foreign-free sig-data)
          (values pub-bytes sig-bytes))))))

(defun signal-session-encrypt (jid device-id plaintext-bytes)
  "Encrypt PLAINTEXT-BYTES for JID/DEVICE-ID using existing session.
   Returns (values ciphertext-bytes message-type) where message-type is
   +ciphertext-signal-type+ or +ciphertext-prekey-type+."
  (let ((len (length plaintext-bytes)))
    (cffi:with-foreign-objects ((ct-ptr :pointer) (ct-len :size) (msg-type :int))
      (with-foreign-byte-array (pt plaintext-bytes)
        (let ((result (%signal-encrypt jid device-id pt len ct-ptr ct-len msg-type)))
          (unless (zerop result)
            (error "Signal encrypt failed: ~A" result))
          (let* ((size (cffi:mem-ref ct-len :size))
                 (data (cffi:mem-ref ct-ptr :pointer))
                 (bytes (cffi:foreign-array-to-lisp data `(:array :uint8 ,size))))
            (cffi:foreign-free data)
            (values bytes (cffi:mem-ref msg-type :int))))))))

(defun signal-session-decrypt (jid device-id ciphertext-bytes &key prekey-p)
  "Decrypt CIPHERTEXT-BYTES from JID/DEVICE-ID.
   PREKEY-P should be T if this is a PreKeySignalMessage."
  (let ((len (length ciphertext-bytes)))
    (cffi:with-foreign-objects ((pt-ptr :pointer) (pt-len :size))
      (with-foreign-byte-array (ct ciphertext-bytes)
        (let ((result (%signal-decrypt jid device-id ct len (if prekey-p 1 0) pt-ptr pt-len)))
          (unless (zerop result)
            (error "Signal decrypt failed: ~A" result))
          (let* ((size (cffi:mem-ref pt-len :size))
                 (data (cffi:mem-ref pt-ptr :pointer))
                 (bytes (cffi:foreign-array-to-lisp data `(:array :uint8 ,size))))
            (cffi:foreign-free data)
            bytes))))))

(defun signal-build-session (jid device-id registration-id
                             prekey-id prekey-public
                             signed-prekey-id signed-prekey-public
                             signed-prekey-signature identity-key)
  "Build a session from a fetched OMEMO bundle.
   All key arguments are byte arrays (with 0x05 DJB prefix if applicable)."
  (let ((pk-len (length prekey-public))
        (spk-len (length signed-prekey-public))
        (sig-len (length signed-prekey-signature))
        (ik-len (length identity-key)))
    (with-foreign-byte-array (pk prekey-public)
      (with-foreign-byte-array (spk signed-prekey-public)
        (with-foreign-byte-array (sig signed-prekey-signature)
          (with-foreign-byte-array (ik identity-key)
            (let ((result (%signal-build-session jid device-id registration-id
                                                 prekey-id pk pk-len
                                                 signed-prekey-id spk spk-len
                                                 sig sig-len
                                                 ik ik-len)))
              (unless (zerop result)
                (error "Failed to build session: ~A" result))
              t)))))))

(defun signal-has-session-p (jid device-id)
  "Check if we have an established session with JID/DEVICE-ID."
  (plusp (%signal-has-session jid device-id)))

(defun signal-get-identity-public ()
  "Get our identity public key bytes."
  (cffi:with-foreign-objects ((pub-ptr :pointer) (pub-len :size))
    (let ((result (%signal-get-identity-public pub-ptr pub-len)))
      (unless (zerop result)
        (error "Failed to get identity public key: ~A" result))
      (let* ((size (cffi:mem-ref pub-len :size))
             (data (cffi:mem-ref pub-ptr :pointer))
             (bytes (cffi:foreign-array-to-lisp data `(:array :uint8 ,size))))
        (cffi:foreign-free data)
        bytes))))
