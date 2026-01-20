;;;; sasl.lisp - SASL authentication for XMPP
;;;;
;;;; Implements SASL PLAIN and SCRAM-SHA-1 mechanisms.

(in-package #:clabber)

;;; ============================================================
;;; SASL PLAIN
;;; ============================================================

(defun sasl-plain-response (authzid authcid password)
  "Generate SASL PLAIN response: base64(authzid NUL authcid NUL password)."
  (let* ((plain-str (format nil "~a~c~a~c~a" 
                            (or authzid "") 
                            #\Nul 
                            authcid 
                            #\Nul 
                            password))
         (octets (babel:string-to-octets plain-str :encoding :utf-8)))
    (cl-base64:usb8-array-to-base64-string octets)))

(defun sasl-plain-auth-element (username password)
  "Create the SASL PLAIN auth element."
  (make-xml-element "auth"
                    :namespace +ns-sasl+
                    :attributes '(("mechanism" . "PLAIN"))
                    :text (sasl-plain-response nil username password)))

;;; ============================================================
;;; SASL SCRAM-SHA-1
;;; ============================================================

(defun generate-nonce (&optional (length 24))
  "Generate a random nonce string."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (with-output-to-string (s)
      (dotimes (i length)
        (write-char (char chars (random (length chars))) s)))))

(defun hmac-sha1 (key message)
  "Compute HMAC-SHA1."
  (let ((hmac (ironclad:make-hmac key :sha1)))
    (ironclad:update-hmac hmac message)
    (ironclad:hmac-digest hmac)))

(defun sha1-digest (data)
  "Compute SHA1 digest."
  (ironclad:digest-sequence :sha1 data))

(defun pbkdf2-sha1 (password salt iterations key-length)
  "Derive key using PBKDF2 with SHA1."
  (ironclad:derive-key 
   (ironclad:make-kdf :pbkdf2 :digest :sha1)
   (babel:string-to-octets password :encoding :utf-8)
   salt
   iterations
   key-length))

(defun xor-bytes (a b)
  "XOR two byte arrays."
  (map 'vector #'logxor a b))

(defclass scram-state ()
  ((username    :initarg :username :accessor scram-username)
   (password    :initarg :password :accessor scram-password)
   (client-nonce :initform (generate-nonce) :accessor scram-client-nonce)
   (server-nonce :initform nil :accessor scram-server-nonce)
   (salt        :initform nil :accessor scram-salt)
   (iterations  :initform nil :accessor scram-iterations)
   (auth-message :initform nil :accessor scram-auth-message)
   (salted-password :initform nil :accessor scram-salted-password))
  (:documentation "State for SCRAM-SHA-1 authentication."))

(defun scram-client-first-bare (state)
  "Generate the client-first-message-bare."
  (format nil "n=~a,r=~a" (scram-username state) (scram-client-nonce state)))

(defun scram-client-first (state)
  "Generate the full client-first-message."
  (format nil "n,,~a" (scram-client-first-bare state)))

(defun scram-parse-server-first (message)
  "Parse server-first-message. Returns (values nonce salt iterations)."
  (let ((parts (split-string message #\,)))
    (let ((r (find-if (lambda (p) (and (> (length p) 2) (string= (subseq p 0 2) "r="))) parts))
          (s (find-if (lambda (p) (and (> (length p) 2) (string= (subseq p 0 2) "s="))) parts))
          (i (find-if (lambda (p) (and (> (length p) 2) (string= (subseq p 0 2) "i="))) parts)))
      (values (when r (subseq r 2))
              (when s (cl-base64:base64-string-to-usb8-array (subseq s 2)))
              (when i (parse-integer (subseq i 2)))))))

(defun scram-client-final (state server-first)
  "Generate client-final-message given server-first-message."
  (multiple-value-bind (nonce salt iterations)
      (scram-parse-server-first server-first)
    (setf (scram-server-nonce state) nonce
          (scram-salt state) salt
          (scram-iterations state) iterations)
    ;; Compute salted password
    (let* ((salted-password (pbkdf2-sha1 (scram-password state) salt iterations 20))
           (client-key (hmac-sha1 salted-password 
                                  (babel:string-to-octets "Client Key" :encoding :utf-8)))
           (stored-key (sha1-digest client-key))
           (client-first-bare (scram-client-first-bare state))
           (client-final-without-proof (format nil "c=biws,r=~a" nonce))
           (auth-message (format nil "~a,~a,~a" 
                                 client-first-bare 
                                 server-first 
                                 client-final-without-proof))
           (client-signature (hmac-sha1 stored-key 
                                        (babel:string-to-octets auth-message :encoding :utf-8)))
           (client-proof (xor-bytes client-key client-signature))
           (proof-b64 (cl-base64:usb8-array-to-base64-string client-proof)))
      (setf (scram-salted-password state) salted-password
            (scram-auth-message state) auth-message)
      (format nil "~a,p=~a" client-final-without-proof proof-b64))))

(defun scram-verify-server-final (state server-final)
  "Verify server-final-message. Returns t if valid."
  (let* ((v-part (find-if (lambda (p) (and (> (length p) 2) (string= (subseq p 0 2) "v=")))
                          (split-string server-final #\,)))
         (server-signature-b64 (when v-part (subseq v-part 2)))
         (server-key (hmac-sha1 (scram-salted-password state)
                                (babel:string-to-octets "Server Key" :encoding :utf-8)))
         (expected-signature (hmac-sha1 server-key
                                        (babel:string-to-octets (scram-auth-message state) 
                                                                :encoding :utf-8)))
         (expected-b64 (cl-base64:usb8-array-to-base64-string expected-signature)))
    (string= server-signature-b64 expected-b64)))

(defun split-string (string delimiter)
  "Split a string by delimiter character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

;;; ============================================================
;;; SASL Authentication Flow
;;; ============================================================

(defun xmpp-sasl-auth (stream username password &key (mechanism :plain))
  "Perform SASL authentication on the stream."
  (ecase mechanism
    (:plain (xmpp-sasl-plain stream username password))
    (:scram-sha-1 (xmpp-sasl-scram-sha-1 stream username password))))

(defun xmpp-sasl-plain (stream username password)
  "Perform SASL PLAIN authentication."
  (xmpp-stream-send stream (sasl-plain-auth-element username password))
  (let ((response (xmpp-stream-read-element stream)))
    (cond
      ((and response (string= (xml-name response) "success"))
       t)
      ((and response (string= (xml-name response) "failure"))
       (error "SASL PLAIN authentication failed"))
      (t
       (error "Unexpected SASL response: ~a" response)))))

(defun xmpp-sasl-scram-sha-1 (stream username password)
  "Perform SASL SCRAM-SHA-1 authentication."
  (let ((state (make-instance 'scram-state 
                              :username username 
                              :password password)))
    ;; Send client-first
    (let ((client-first (scram-client-first state)))
      (xmpp-stream-send stream
                        (make-xml-element "auth"
                                          :namespace +ns-sasl+
                                          :attributes '(("mechanism" . "SCRAM-SHA-1"))
                                          :text (cl-base64:string-to-base64-string client-first))))
    ;; Read challenge (server-first)
    (let ((challenge (xmpp-stream-read-element stream)))
      (unless (and challenge (string= (xml-name challenge) "challenge"))
        (error "Expected SCRAM challenge, got: ~a" challenge))
      (let* ((server-first (cl-base64:base64-string-to-string (xml-text challenge)))
             (client-final (scram-client-final state server-first)))
        ;; Send client-final
        (xmpp-stream-send stream
                          (make-xml-element "response"
                                            :namespace +ns-sasl+
                                            :text (cl-base64:string-to-base64-string client-final)))
        ;; Read success (with server-final)
        (let ((result (xmpp-stream-read-element stream)))
          (cond
            ((and result (string= (xml-name result) "success"))
             ;; Optionally verify server signature
             (let ((server-final (ignore-errors 
                                   (cl-base64:base64-string-to-string (xml-text result)))))
               (when server-final
                 (unless (scram-verify-server-final state server-final)
                   (warn "Server signature verification failed"))))
             t)
            ((and result (string= (xml-name result) "failure"))
             (error "SASL SCRAM-SHA-1 authentication failed"))
            (t
             (error "Unexpected SASL response: ~a" result))))))))
