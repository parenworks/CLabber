(in-package #:clabber.xmpp)

;;; XMPP XML stream handling over TCP/TLS

;;; ============================================================
;;; XMPP Stream Class
;;; ============================================================

(defclass xmpp-stream ()
  ((socket       :initform nil :accessor stream-socket)
   (ssl-stream   :initform nil :accessor stream-ssl)
   (input-stream :initform nil :accessor stream-input)
   (output-stream :initform nil :accessor stream-output)
   (hostname     :initarg :hostname :accessor stream-hostname)
   (port         :initarg :port :accessor stream-port :initform 5222)
   (domain       :initarg :domain :accessor stream-domain)
   (connected-p  :initform nil :accessor stream-connected-p)
   (tls-p        :initform nil :accessor stream-tls-p)
   (stream-id    :initform nil :accessor stream-id)
   (features     :initform nil :accessor stream-features))
  (:documentation "Manages an XMPP XML stream over TCP/TLS."))

(defmethod print-object ((s xmpp-stream) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a:~d ~a"
            (stream-hostname s) (stream-port s)
            (if (stream-connected-p s) "connected" "disconnected"))))

;;; ============================================================
;;; Connection Management
;;; ============================================================

(defun xmpp-stream-connect (hostname port &key domain)
  "Create a new XMPP stream and connect via plain TCP."
  (let ((stream (make-instance 'xmpp-stream
                               :hostname hostname :port port
                               :domain (or domain hostname))))
    (handler-case
        (let ((sock (usocket:socket-connect hostname port
                                            :element-type '(unsigned-byte 8))))
          (setf (stream-socket stream) sock)
          (setf (stream-input stream) (usocket:socket-stream sock))
          (setf (stream-output stream) (usocket:socket-stream sock))
          (setf (stream-connected-p stream) t)
          stream)
      (error (e)
        (error "Failed to connect to ~a:~d: ~a" hostname port e)))))

(defun xmpp-stream-connect-tls (hostname port &key domain)
  "Create a new XMPP stream with direct TLS (port 5223 style)."
  (let ((stream (make-instance 'xmpp-stream
                               :hostname hostname :port port
                               :domain (or domain hostname))))
    (handler-case
        (let* ((sock (usocket:socket-connect hostname port
                                             :element-type '(unsigned-byte 8)))
               (sock-stream (usocket:socket-stream sock))
               (ssl-stream (cl+ssl:make-ssl-client-stream
                            sock-stream :hostname hostname :verify nil)))
          (setf (stream-socket stream) sock
                (stream-ssl stream) ssl-stream
                (stream-input stream) ssl-stream
                (stream-output stream) ssl-stream
                (stream-connected-p stream) t
                (stream-tls-p stream) t)
          stream)
      (error (e)
        (error "Failed to connect TLS to ~a:~d: ~a" hostname port e)))))

(defun xmpp-stream-disconnect (stream)
  "Disconnect and close the XMPP stream."
  (when (stream-connected-p stream)
    (handler-case
        (progn
          (xmpp-stream-send-raw stream (make-stream-close))
          (when (stream-ssl stream)
            (close (stream-ssl stream)))
          (when (stream-socket stream)
            (usocket:socket-close (stream-socket stream))))
      (error () nil))
    (setf (stream-connected-p stream) nil
          (stream-socket stream) nil
          (stream-ssl stream) nil
          (stream-input stream) nil
          (stream-output stream) nil)))

;;; ============================================================
;;; TLS Upgrade (STARTTLS)
;;; ============================================================

(defun xmpp-stream-starttls (stream)
  "Upgrade the connection to TLS using STARTTLS."
  (unless (stream-tls-p stream)
    (xmpp-stream-send-raw stream "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")
    (let ((response (xmpp-stream-read-element stream)))
      (unless (and response (string= (xml-name response) "proceed"))
        (error "STARTTLS failed: ~a" response)))
    (let* ((sock-stream (usocket:socket-stream (stream-socket stream)))
           (ssl-stream (cl+ssl:make-ssl-client-stream
                        sock-stream
                        :hostname (stream-hostname stream)
                        :verify nil)))
      (setf (stream-ssl stream) ssl-stream
            (stream-input stream) ssl-stream
            (stream-output stream) ssl-stream
            (stream-tls-p stream) t))
    t))

;;; ============================================================
;;; Stream Opening
;;; ============================================================

(defun xmpp-stream-open (stream)
  "Send the opening stream tag and read the server's response features."
  (xmpp-stream-send-raw stream (make-stream-open (stream-domain stream)))
  (let ((features (xmpp-stream-read-stream-start stream)))
    (setf (stream-features stream) features)
    features))

(defun xmpp-stream-restart (stream)
  "Restart the XML stream (required after TLS and auth)."
  (xmpp-stream-open stream))

;;; ============================================================
;;; Raw I/O
;;; ============================================================

(defun xmpp-stream-send-raw (stream data)
  "Send raw data to the stream."
  (let ((out (stream-output stream)))
    (write-sequence (babel:string-to-octets data :encoding :utf-8) out)
    (force-output out)))

(defun xmpp-stream-send (stream element)
  "Send an XML element to the stream."
  (xmpp-stream-send-raw stream (serialize-xml element)))

;;; ============================================================
;;; XML Reading
;;; ============================================================

(defun xmpp-stream-read-stream-start (stream)
  "Read the opening stream:stream tag and initial features."
  (let ((buffer (make-array 8192 :element-type 'character :fill-pointer 0))
        (in (stream-input stream)))
    (loop for byte = (read-byte in nil nil)
          while byte
          for c = (code-char byte)
          do (vector-push-extend c buffer)
          when (char= c #\>)
            do (let ((str (coerce buffer 'string)))
                 (when (and (null (stream-id stream))
                            (search "<stream:stream" str))
                   (let ((id-start (or (search "id='" str) (search "id=\"" str))))
                     (when id-start
                       (let* ((quote-char (char str (+ id-start 3)))
                              (id-begin (+ id-start 4))
                              (id-end (position quote-char str :start id-begin)))
                         (when id-end
                           (setf (stream-id stream)
                                 (subseq str id-begin id-end)))))))
                 (when (search "</stream:features>" str)
                   (return (parse-stream-features str)))))))

(defun parse-stream-features (xml-string)
  "Parse stream:features from XML string. Returns a plist of features."
  (let ((features nil))
    (when (search "<starttls" xml-string)
      (push :starttls features)
      (when (search "<required" xml-string)
        (push :starttls-required features)))
    (when (search "<mechanisms" xml-string)
      (let ((mechs nil))
        (when (search ">PLAIN<" xml-string) (push :plain mechs))
        (when (search ">SCRAM-SHA-1<" xml-string) (push :scram-sha-1 mechs))
        (when (search ">SCRAM-SHA-256<" xml-string) (push :scram-sha-256 mechs))
        (when (search ">EXTERNAL<" xml-string) (push :external mechs))
        (push (cons :sasl-mechanisms mechs) features)))
    (when (search "<bind" xml-string)
      (push :bind features))
    (when (search "<session" xml-string)
      (push :session features))
    features))

(defun xmpp-stream-read-element (stream)
  "Read a complete XML element from the stream."
  (let ((buffer (make-array 4096 :element-type 'character :fill-pointer 0))
        (in (stream-input stream))
        (depth 0)
        (in-tag nil)
        (root-name nil))
    (loop for byte = (read-byte in nil nil)
          while byte
          for c = (code-char byte)
          do (vector-push-extend c buffer)
          when (char= c #\<) do (setf in-tag t)
          when (and in-tag (char= c #\>))
            do (let* ((str (coerce buffer 'string))
                      (tag-start (position #\< str :from-end t))
                      (tag-content (when tag-start (subseq str (1+ tag-start)))))
                 (setf in-tag nil)
                 (when tag-content
                   (cond
                     ;; Self-closing tag
                     ((and (> (length tag-content) 1)
                           (char= (char tag-content (- (length tag-content) 2)) #\/))
                      (if (null root-name)
                          (return (parse-full-element str))
                          nil))
                     ;; Closing tag
                     ((char= (char tag-content 0) #\/)
                      (decf depth)
                      (when (<= depth 0)
                        (return (parse-full-element str))))
                     ;; Opening tag
                     (t
                      (when (null root-name)
                        (let* ((space-pos (position #\Space tag-content))
                               (gt-pos (position #\> tag-content))
                               (slash-pos (position #\/ tag-content))
                               (end-pos (or space-pos slash-pos (when gt-pos (1- gt-pos))
                                            (length tag-content))))
                          (setf root-name (subseq tag-content 0 end-pos))))
                      (incf depth))))))))
