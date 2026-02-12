;;;; omemo-tests.lisp - Unit tests for OMEMO encryption
;;;;
;;;; Tests the crypto primitives, key conversion, X3DH, Double Ratchet,
;;;; and full OMEMO encrypt/decrypt flow without needing a live XMPP connection.

(in-package #:clabber)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defvar *test-pass-count* 0)
(defvar *test-fail-count* 0)
(defvar *test-errors* nil)

(defun test-reset ()
  "Reset test counters."
  (setf *test-pass-count* 0
        *test-fail-count* 0
        *test-errors* nil))

(defun test-assert (name condition)
  "Assert that CONDITION is true. NAME describes the test."
  (if condition
      (progn
        (incf *test-pass-count*)
        (format t "  PASS: ~a~%" name))
      (progn
        (incf *test-fail-count*)
        (push name *test-errors*)
        (format t "  FAIL: ~a~%" name))))

(defun test-assert-equal (name expected actual &key (test #'equalp))
  "Assert that EXPECTED equals ACTUAL."
  (if (funcall test expected actual)
      (progn
        (incf *test-pass-count*)
        (format t "  PASS: ~a~%" name))
      (progn
        (incf *test-fail-count*)
        (push (format nil "~a (expected ~a, got ~a)" name expected actual) *test-errors*)
        (format t "  FAIL: ~a~%    expected: ~a~%    got:      ~a~%" name expected actual))))

(defun test-summary ()
  "Print test summary."
  (format t "~%========================================~%")
  (format t "Tests: ~d passed, ~d failed~%" *test-pass-count* *test-fail-count*)
  (when *test-errors*
    (format t "~%Failures:~%")
    (dolist (err (reverse *test-errors*))
      (format t "  - ~a~%" err)))
  (format t "========================================~%")
  (zerop *test-fail-count*))

;;; ============================================================
;;; Test: Byte Conversion Utilities
;;; ============================================================

(defun test-byte-conversions ()
  "Test little-endian byte conversion round-trips."
  (format t "~%--- Byte Conversion Tests ---~%")
  ;; Round-trip: integer -> bytes -> integer
  (let* ((n 12345678)
         (bytes (integer-to-bytes-le n 4))
         (result (bytes-to-integer-le bytes)))
    (test-assert-equal "integer->bytes->integer round-trip" n result))
  ;; Zero
  (let* ((bytes (integer-to-bytes-le 0 32))
         (result (bytes-to-integer-le bytes)))
    (test-assert-equal "zero round-trip" 0 result))
  ;; Max 32-bit
  (let* ((n #xFFFFFFFF)
         (bytes (integer-to-bytes-le n 4))
         (result (bytes-to-integer-le bytes)))
    (test-assert-equal "max-u32 round-trip" n result))
  ;; Large number (255 bits)
  (let* ((n (1- (expt 2 255)))
         (bytes (integer-to-bytes-le n 32))
         (result (bytes-to-integer-le bytes)))
    (test-assert-equal "large number round-trip" n result)))

;;; ============================================================
;;; Test: Modular Inverse
;;; ============================================================

(defun test-mod-inverse ()
  "Test modular inverse computation."
  (format t "~%--- Modular Inverse Tests ---~%")
  ;; 3 * inv(3) mod 7 = 1
  (let ((inv (mod-inverse 3 7)))
    (test-assert-equal "3 * inv(3) mod 7 = 1" 1 (mod (* 3 inv) 7)))
  ;; With curve25519 prime
  (let* ((p +curve25519-prime+)
         (a 42)
         (inv (mod-inverse a p)))
    (test-assert-equal "42 * inv(42) mod p = 1" 1 (mod (* a inv) p))))

;;; ============================================================
;;; Test: Ed25519 <-> X25519 Key Conversion
;;; ============================================================

(defun test-key-conversion ()
  "Test Ed25519 to X25519 key conversion."
  (format t "~%--- Key Conversion Tests ---~%")
  ;; Generate an Ed25519 keypair and convert
  (multiple-value-bind (ed-priv ed-pub) (generate-ed25519-keypair)
    ;; Conversion should produce 32-byte results
    (let ((x-pub (ed25519-public-to-x25519 ed-pub))
          (x-priv (ed25519-private-to-x25519 ed-priv)))
      (test-assert-equal "X25519 public key is 32 bytes" 32 (length x-pub))
      (test-assert-equal "X25519 private key is 32 bytes" 32 (length x-priv))
      ;; Clamping checks on private key
      (test-assert "X25519 private key bit 0,1,2 cleared" 
                   (zerop (logand (aref x-priv 0) #x07)))
      (test-assert "X25519 private key bit 255 cleared"
                   (zerop (logand (aref x-priv 31) #x80)))
      (test-assert "X25519 private key bit 254 set"
                   (not (zerop (logand (aref x-priv 31) #x40))))
      ;; Converted key should not be all zeros
      (test-assert "X25519 public key is not all zeros"
                   (some (lambda (b) (not (zerop b))) x-pub))
      ;; Conversion should be deterministic
      (let ((x-pub2 (ed25519-public-to-x25519 ed-pub)))
        (test-assert "Key conversion is deterministic" (equalp x-pub x-pub2))))))

;;; ============================================================
;;; Test: X25519 Diffie-Hellman
;;; ============================================================

(defun test-x25519-dh ()
  "Test X25519 Diffie-Hellman key agreement."
  (format t "~%--- X25519 DH Tests ---~%")
  ;; Generate two keypairs
  (multiple-value-bind (priv-a pub-a) (generate-x25519-keypair)
    (multiple-value-bind (priv-b pub-b) (generate-x25519-keypair)
      ;; DH(a, B) should equal DH(b, A)
      (let ((shared-ab (x25519-dh priv-a pub-b))
            (shared-ba (x25519-dh priv-b pub-a)))
        (test-assert "DH(a,B) = DH(b,A)" (equalp shared-ab shared-ba))
        (test-assert-equal "Shared secret is 32 bytes" 32 (length shared-ab))
        (test-assert "Shared secret is not all zeros"
                     (some (lambda (b) (not (zerop b))) shared-ab))))))

;;; ============================================================
;;; Test: HKDF-SHA256
;;; ============================================================

(defun test-hkdf ()
  "Test HKDF-SHA256 key derivation."
  (format t "~%--- HKDF Tests ---~%")
  (let* ((ikm (generate-random-bytes 32))
         (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (info (babel:string-to-octets "test"))
         (key1 (hkdf-sha256 ikm salt info 32))
         (key2 (hkdf-sha256 ikm salt info 32)))
    (test-assert-equal "HKDF output is 32 bytes" 32 (length key1))
    (test-assert "HKDF is deterministic" (equalp key1 key2))
    ;; Different info should give different output
    (let ((key3 (hkdf-sha256 ikm salt (babel:string-to-octets "other") 32)))
      (test-assert "Different info gives different key" (not (equalp key1 key3))))))

;;; ============================================================
;;; Test: X3DH Key Agreement
;;; ============================================================

(defun test-x3dh ()
  "Test X3DH key agreement between initiator and responder."
  (format t "~%--- X3DH Tests ---~%")
  ;; Generate Alice's (initiator) keys
  (multiple-value-bind (alice-ik-priv alice-ik-pub) (generate-ed25519-keypair)
    (declare (ignore alice-ik-pub))
    (multiple-value-bind (alice-ek-priv alice-ek-pub) (generate-x25519-keypair)
      (declare (ignore alice-ek-pub))
      ;; Generate Bob's (responder) keys
      (multiple-value-bind (bob-ik-priv bob-ik-pub) (generate-ed25519-keypair)
        (declare (ignore bob-ik-priv))
        (multiple-value-bind (bob-spk-priv bob-spk-pub) (generate-x25519-keypair)
          (declare (ignore bob-spk-priv))
          (multiple-value-bind (bob-opk-priv bob-opk-pub) (generate-x25519-keypair)
            (declare (ignore bob-opk-priv))
            ;; X3DH with one-time prekey
            (let ((alice-secret (x3dh-initiator alice-ik-priv alice-ek-priv
                                               bob-ik-pub bob-spk-pub bob-opk-pub)))
              (test-assert-equal "X3DH shared secret is 32 bytes" 32 (length alice-secret))
              (test-assert "X3DH shared secret is not all zeros"
                           (some (lambda (b) (not (zerop b))) alice-secret)))
            ;; X3DH without one-time prekey
            (let ((alice-secret-no-opk (x3dh-initiator alice-ik-priv alice-ek-priv
                                                       bob-ik-pub bob-spk-pub nil)))
              (test-assert-equal "X3DH no-OPK secret is 32 bytes" 32
                                 (length alice-secret-no-opk)))))))))

;;; ============================================================
;;; Test: X3DH with proper public key exchange
;;; ============================================================

(defun test-x3dh-proper ()
  "Test X3DH with proper public/private key separation."
  (format t "~%--- X3DH Proper Key Exchange Tests ---~%")
  ;; Alice (initiator)
  (multiple-value-bind (alice-ik-priv alice-ik-pub) (generate-ed25519-keypair)
    ;; Bob (responder)
    (multiple-value-bind (bob-ik-priv bob-ik-pub) (generate-ed25519-keypair)
      (multiple-value-bind (bob-spk-priv bob-spk-pub) (generate-x25519-keypair)
        (multiple-value-bind (bob-opk-priv bob-opk-pub) (generate-x25519-keypair)
          ;; Alice generates ephemeral keypair
          (multiple-value-bind (alice-ek-priv alice-ek-pub) (generate-x25519-keypair)
            ;; Alice computes shared secret using:
            ;;   her identity private + ephemeral private
            ;;   Bob's identity public + signed prekey public + one-time prekey public
            (let ((alice-secret (x3dh-initiator alice-ik-priv alice-ek-priv
                                               bob-ik-pub bob-spk-pub bob-opk-pub)))
              ;; Bob computes shared secret using:
              ;;   his identity private + signed prekey private + one-time prekey private
              ;;   Alice's identity public + ephemeral PUBLIC key
              (let ((bob-secret (x3dh-responder bob-ik-priv bob-spk-priv bob-opk-priv
                                                alice-ik-pub alice-ek-pub)))
                (test-assert "X3DH: Alice and Bob derive same secret"
                             (equalp alice-secret bob-secret))))))))))

;;; ============================================================
;;; Test: Double Ratchet Chain KDF
;;; ============================================================

(defun test-double-ratchet-kdf ()
  "Test Double Ratchet KDF chain operations."
  (format t "~%--- Double Ratchet KDF Tests ---~%")
  (let ((chain-key (generate-random-bytes 32)))
    ;; Step chain forward
    (multiple-value-bind (next-ck msg-key) (kdf-chain chain-key)
      (test-assert-equal "Next chain key is 32 bytes" 32 (length next-ck))
      (test-assert-equal "Message key is 32 bytes" 32 (length msg-key))
      (test-assert "Chain key and message key differ" (not (equalp next-ck msg-key)))
      (test-assert "New chain key differs from old" (not (equalp next-ck chain-key)))
      ;; Deterministic
      (multiple-value-bind (next-ck2 msg-key2) (kdf-chain chain-key)
        (test-assert "KDF chain is deterministic (chain key)" (equalp next-ck next-ck2))
        (test-assert "KDF chain is deterministic (msg key)" (equalp msg-key msg-key2)))))
  ;; Root KDF
  (let ((root-key (generate-random-bytes 32))
        (dh-output (generate-random-bytes 32)))
    (multiple-value-bind (new-root new-chain) (kdf-root root-key dh-output)
      (test-assert-equal "New root key is 32 bytes" 32 (length new-root))
      (test-assert-equal "New chain key is 32 bytes" 32 (length new-chain))
      (test-assert "Root and chain keys differ" (not (equalp new-root new-chain))))))

;;; ============================================================
;;; Test: Device Generation and Serialization
;;; ============================================================

(defun test-device-generation ()
  "Test OMEMO device generation and serialization."
  (format t "~%--- Device Generation Tests ---~%")
  (let ((device (generate-omemo-device)))
    (test-assert "Device ID is positive" (> (device-id device) 0))
    (test-assert "Device has identity key" (not (null (device-identity-key device))))
    (test-assert "Device has signed prekey" (not (null (device-signed-prekey device))))
    (test-assert-equal "Device has 100 prekeys" 100 
                       (hash-table-count (device-prekeys device)))
    ;; Signed prekey signature should verify
    (let* ((ik (device-identity-key device))
           (spk (device-signed-prekey device))
           (valid (verify-ed25519 (identity-public-key ik)
                                  (signed-prekey-public spk)
                                  (signed-prekey-signature spk))))
      (test-assert "Signed prekey signature verifies" valid))
    ;; Serialization round-trip
    (let* ((serialized (serialize-omemo-device device))
           (restored (deserialize-omemo-device serialized)))
      (test-assert-equal "Device ID survives serialization" 
                         (device-id device) (device-id restored))
      (test-assert "Identity key survives serialization"
                   (equalp (identity-public-key (device-identity-key device))
                           (identity-public-key (device-identity-key restored))))
      (test-assert-equal "Prekey count survives serialization"
                         (hash-table-count (device-prekeys device))
                         (hash-table-count (device-prekeys restored))))))

;;; ============================================================
;;; Test: Full OMEMO Encrypt/Decrypt Flow
;;; ============================================================

(defun test-omemo-encrypt-decrypt ()
  "Test full OMEMO message encryption and decryption between two simulated devices."
  (format t "~%--- Full OMEMO Encrypt/Decrypt Tests ---~%")
  ;; Create two devices (Alice and Bob)
  (let* ((alice-device (generate-omemo-device))
         (bob-device (generate-omemo-device))
         (bob-jid "bob@example.com")
         ;; Save/restore global state
         (old-device *omemo-device*)
         (old-sessions *omemo-sessions*)
         (old-device-lists *remote-device-lists*)
         (old-bundles *remote-bundles*))
    (unwind-protect
        (progn
          ;; Reset global state for test
          (setf *omemo-sessions* (make-hash-table :test 'equal))
          (setf *remote-device-lists* (make-hash-table :test 'equal))
          (setf *remote-bundles* (make-hash-table :test 'equal))
          
          ;; === Alice's perspective: establish session with Bob ===
          (setf *omemo-device* alice-device)
          
          ;; Alice knows Bob's device list
          (cache-device-list bob-jid (list (device-id bob-device)))
          
          ;; Alice has Bob's bundle
          (let ((bob-bundle (make-instance 'remote-bundle
                                           :jid bob-jid
                                           :device-id (device-id bob-device)
                                           :identity-key (identity-public-key 
                                                          (device-identity-key bob-device))
                                           :signed-prekey-id (signed-prekey-id 
                                                              (device-signed-prekey bob-device))
                                           :signed-prekey (signed-prekey-public 
                                                           (device-signed-prekey bob-device))
                                           :signed-prekey-sig (signed-prekey-signature 
                                                               (device-signed-prekey bob-device))
                                           :prekeys (let ((ht (make-hash-table)))
                                                      (maphash (lambda (id pk)
                                                                 (setf (gethash id ht) 
                                                                       (prekey-public pk)))
                                                               (device-prekeys bob-device))
                                                      ht))))
            (cache-bundle bob-bundle)
            
            ;; Alice creates session with Bob
            (multiple-value-bind (session eph-pub)
                (create-session-as-initiator bob-jid (device-id bob-device) bob-bundle)
              (declare (ignore eph-pub))
              (test-assert "Alice created session" (not (null session)))
              
              ;; Alice encrypts a message
              (let ((encrypted-el (omemo-encrypt-message bob-jid "Hello Bob! This is encrypted.")))
                (test-assert "Alice produced encrypted element" (not (null encrypted-el)))
                (test-assert "Encrypted element is named 'encrypted'"
                             (string= "encrypted" (xml-name encrypted-el)))
                
                ;; Verify structure
                (let ((header (xml-child encrypted-el "header"))
                      (payload (xml-child encrypted-el "payload")))
                  (test-assert "Has header element" (not (null header)))
                  (test-assert "Has payload element" (not (null payload)))
                  (test-assert "Header has sid attribute" 
                               (not (null (xml-attr header "sid"))))
                  (test-assert-equal "Header sid matches Alice's device" 
                                     (princ-to-string (device-id alice-device))
                                     (xml-attr header "sid")))))))
      ;; Restore global state
      (setf *omemo-device* old-device
            *omemo-sessions* old-sessions
            *remote-device-lists* old-device-lists
            *remote-bundles* old-bundles))))

;;; ============================================================
;;; Test: XML Parsing of OMEMO Elements
;;; ============================================================

(defun test-omemo-xml-parsing ()
  "Test parsing of OMEMO XML structures."
  (format t "~%--- OMEMO XML Parsing Tests ---~%")
  ;; Build a mock OMEMO encrypted element
  (let* ((key-el (make-xml-element "key"
                                   :attributes '(("rid" . "12345") ("prekey" . "true"))
                                   :text "AQIDBA=="))  ; base64 of #(1 2 3 4)
         (iv-el (make-xml-element "iv" :text "AQIDBAUG"))
         (header-el (make-xml-element "header"
                                      :attributes '(("sid" . "67890"))
                                      :children (list key-el iv-el)))
         (payload-el (make-xml-element "payload" :text "BwgJCg=="))
         (encrypted-el (make-xml-element "encrypted"
                                         :namespace "urn:xmpp:omemo:2"
                                         :children (list header-el payload-el))))
    ;; Parse it
    (let ((msg (parse-omemo-encrypted encrypted-el)))
      (test-assert "Parsed OMEMO message" (not (null msg)))
      (when msg
        (test-assert-equal "Sender device ID" 67890 (omemo-sender-device-id msg))
        (test-assert "Has keys" (not (null (omemo-keys msg))))
        (test-assert "Has payload" (not (null (omemo-payload msg))))
        (let ((key (find-key-for-device msg 12345)))
          (test-assert "Found key for device 12345" (not (null key)))
          (when key
            (test-assert "Key is marked as prekey" (key-is-prekey key))
            (test-assert-equal "Key recipient device ID" 12345 
                               (key-recipient-device-id key))))
        ;; No key for unknown device
        (test-assert "No key for device 99999" 
                     (null (find-key-for-device msg 99999)))))))

;;; ============================================================
;;; Test: Ed25519 Signing and Verification
;;; ============================================================

(defun test-ed25519-signing ()
  "Test Ed25519 signing and verification."
  (format t "~%--- Ed25519 Signing Tests ---~%")
  (multiple-value-bind (priv pub) (generate-ed25519-keypair)
    (let* ((message (babel:string-to-octets "test message"))
           (signature (sign-ed25519 priv message)))
      (test-assert "Signature is not nil" (not (null signature)))
      (test-assert "Signature verifies" (verify-ed25519 pub message signature))
      ;; Wrong message should fail
      (test-assert "Wrong message fails verification"
                   (not (verify-ed25519 pub (babel:string-to-octets "wrong") signature)))
      ;; Wrong key should fail
      (multiple-value-bind (priv2 pub2) (generate-ed25519-keypair)
        (declare (ignore priv2))
        (test-assert "Wrong key fails verification"
                     (not (verify-ed25519 pub2 message signature)))))))

;;; ============================================================
;;; Test: AES-256-GCM Encrypt/Decrypt
;;; ============================================================

(defun test-aes-gcm ()
  "Test AES-256-GCM encryption and decryption using our helper functions."
  (format t "~%--- AES-256-GCM Tests ---~%")
  (let* ((key (generate-random-bytes 32))
         (iv (generate-random-bytes 12))
         (plaintext "Hello, OMEMO encryption!")
         (pt-bytes (babel:string-to-octets plaintext :encoding :utf-8)))
    ;; Encrypt
    (multiple-value-bind (ct tag) (aes-gcm-encrypt key iv pt-bytes)
      (test-assert-equal "GCM tag is 16 bytes" 16 (length tag))
      (test-assert "Ciphertext differs from plaintext" (not (equalp ct pt-bytes)))
      ;; Decrypt
      (let ((decrypted (aes-gcm-decrypt key iv ct tag)))
        (test-assert-equal "Decrypted matches original" plaintext
                           (babel:octets-to-string decrypted :encoding :utf-8)))
      ;; Wrong key should fail
      (let ((wrong-key (generate-random-bytes 32)))
        (test-assert "Wrong key fails decryption"
                     (handler-case
                         (progn (aes-gcm-decrypt wrong-key iv ct tag) nil)
                       (error () t)))))))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-omemo-tests ()
  "Run all OMEMO tests."
  (format t "~%========================================~%")
  (format t "Running OMEMO Tests~%")
  (format t "========================================~%")
  (test-reset)
  (handler-case
      (progn
        (test-byte-conversions)
        (test-mod-inverse)
        (test-key-conversion)
        (test-x25519-dh)
        (test-hkdf)
        (test-ed25519-signing)
        (test-aes-gcm)
        (test-x3dh)
        (test-x3dh-proper)
        (test-double-ratchet-kdf)
        (test-device-generation)
        (test-omemo-xml-parsing)
        (test-omemo-encrypt-decrypt))
    (error (e)
      (format t "~%FATAL ERROR during tests: ~a~%" e)))
  (test-summary))
