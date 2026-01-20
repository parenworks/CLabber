;;;; stream.lisp - XMPP XML stream handling over TCP/TLS
;;;;
;;;; Manages the underlying socket connection and XML stream.

(in-package #:clabber)

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
   (features     :initform nil :accessor stream-features)
   (read-buffer  :initform (make-string-output-stream) :accessor stream-read-buffer))
  (:documentation "Manages an XMPP XML stream over TCP/TLS."))

(defmethod print-object ((s xmpp-stream) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a:~d ~a" 
            (stream-hostname s) 
            (stream-port s)
            (if (stream-connected-p s) "connected" "disconnected"))))

;;; ============================================================
;;; Connection Management
;;; ============================================================

(defun xmpp-stream-connect (hostname port &key domain)
  "Create a new XMPP stream and connect to the server."
  (let ((stream (make-instance 'xmpp-stream
                               :hostname hostname
                               :port port
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

(defun xmpp-stream-disconnect (stream)
  "Disconnect and close the XMPP stream."
  (when (stream-connected-p stream)
    (handler-case
        (progn
          ;; Send stream close
          (xmpp-stream-send-raw stream (make-stream-close))
          ;; Close SSL stream (just close the stream, socket close handles the rest)
          (when (stream-ssl stream)
            (close (stream-ssl stream)))
          ;; Close socket
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
    ;; Send starttls request
    (xmpp-stream-send-raw stream "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")
    ;; Wait for proceed
    (let ((response (xmpp-stream-read-element stream)))
      (unless (and response (string= (xml-name response) "proceed"))
        (error "STARTTLS failed: ~a" response)))
    ;; Upgrade to TLS
    (let* ((sock-stream (usocket:socket-stream (stream-socket stream)))
           (ssl-stream (cl+ssl:make-ssl-client-stream 
                        sock-stream
                        :hostname (stream-hostname stream)
                        :verify nil)))  ; TODO: proper cert verification
      (setf (stream-ssl stream) ssl-stream
            (stream-input stream) ssl-stream
            (stream-output stream) ssl-stream
            (stream-tls-p stream) t))
    ;; Must restart stream after TLS
    t))

(defun xmpp-stream-connect-tls (hostname port &key domain)
  "Create a new XMPP stream with direct TLS connection (port 5223 style)."
  (let ((stream (make-instance 'xmpp-stream
                               :hostname hostname
                               :port port
                               :domain (or domain hostname))))
    (handler-case
        (let* ((sock (usocket:socket-connect hostname port
                                             :element-type '(unsigned-byte 8)))
               (sock-stream (usocket:socket-stream sock))
               (ssl-stream (cl+ssl:make-ssl-client-stream
                            sock-stream
                            :hostname hostname
                            :verify nil)))
          (setf (stream-socket stream) sock
                (stream-ssl stream) ssl-stream
                (stream-input stream) ssl-stream
                (stream-output stream) ssl-stream
                (stream-connected-p stream) t
                (stream-tls-p stream) t)
          stream)
      (error (e)
        (error "Failed to connect TLS to ~a:~d: ~a" hostname port e)))))

;;; ============================================================
;;; Stream Opening
;;; ============================================================

(defun xmpp-stream-open (stream)
  "Send the opening stream tag and read the server's response."
  (xmpp-stream-send-raw stream (make-stream-open (stream-domain stream)))
  ;; Read stream:stream response and features
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
;;; XML Reading (Simplified)
;;; ============================================================

(defun read-until-char (stream char &optional (timeout 30))
  "Read from stream until we see the specified character."
  (declare (ignore timeout))
  (let ((buffer (make-string-output-stream)))
    (loop for byte = (read-byte (stream-input stream) nil nil)
          while byte
          for c = (code-char byte)
          do (write-char c buffer)
          until (char= c char))
    (get-output-stream-string buffer)))

(defun xmpp-stream-read-stream-start (stream)
  "Read the opening stream:stream tag and initial features."
  ;; Read until we get the stream:features closing tag
  (let ((buffer (make-array 8192 :element-type 'character :fill-pointer 0))
        (in (stream-input stream)))
    (debug-log "Reading stream start...")
    ;; Read bytes until we see </stream:features>
    (loop for byte = (read-byte in nil nil)
          while byte
          for c = (code-char byte)
          do (vector-push-extend c buffer)
          ;; Check periodically for completion
          when (char= c #\>)
            do (let ((str (coerce buffer 'string)))
                 (debug-log "Buffer so far (~d chars): ~a..." 
                            (length str) 
                            (subseq str 0 (min 100 (length str))))
                 ;; Check for stream:stream opening and extract ID
                 (when (and (null (stream-id stream))
                            (search "<stream:stream" str))
                   (debug-log "Found stream:stream opening")
                   (let ((id-start (or (search "id='" str) (search "id=\"" str))))
                     (when id-start
                       (let* ((quote-char (char str (+ id-start 3)))
                              (id-begin (+ id-start 4))
                              (id-end (position quote-char str :start id-begin)))
                         (when id-end
                           (setf (stream-id stream) 
                                 (subseq str id-begin id-end))
                           (debug-log "Stream ID: ~a" (stream-id stream)))))))
                 ;; Check for features end
                 (when (search "</stream:features>" str)
                   (debug-log "Found stream:features end")
                   (return (parse-stream-features str)))))))

(defun parse-stream-features (xml-string)
  "Parse stream:features from XML string. Returns a plist of features."
  (let ((features nil))
    ;; Check for STARTTLS
    (when (search "<starttls" xml-string)
      (push :starttls features)
      (when (search "<required" xml-string)
        (push :starttls-required features)))
    ;; Check for SASL mechanisms
    (when (search "<mechanisms" xml-string)
      (let ((mechs nil))
        (when (search ">PLAIN<" xml-string) (push :plain mechs))
        (when (search ">SCRAM-SHA-1<" xml-string) (push :scram-sha-1 mechs))
        (when (search ">SCRAM-SHA-256<" xml-string) (push :scram-sha-256 mechs))
        (when (search ">EXTERNAL<" xml-string) (push :external mechs))
        (push (cons :sasl-mechanisms mechs) features)))
    ;; Check for bind
    (when (search "<bind" xml-string)
      (push :bind features))
    ;; Check for session
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
    (debug-log "Reading element...")
    (loop for byte = (read-byte in nil nil)
          while byte
          for c = (code-char byte)
          do (vector-push-extend c buffer)
          ;; Track tag boundaries
          when (char= c #\<) do (setf in-tag t)
          when (and in-tag (char= c #\>))
            do (let* ((str (coerce buffer 'string))
                      (tag-start (position #\< str :from-end t))
                      (tag-content (when tag-start (subseq str (1+ tag-start)))))
                 (setf in-tag nil)
                 (when tag-content
                   (cond
                     ;; Self-closing tag like <foo/>
                     ((and (> (length tag-content) 1)
                           (char= (char tag-content (- (length tag-content) 2)) #\/))
                      (if (null root-name)
                          ;; Single self-closing element
                          (progn
                            (debug-log "Read self-closing element: ~a" 
                                       (subseq str 0 (min 80 (length str))))
                            (return (parse-full-element str)))
                          ;; Nested self-closing, continue
                          nil))
                     ;; Closing tag like </foo>
                     ((char= (char tag-content 0) #\/)
                      (decf depth)
                      (when (<= depth 0)
                        (debug-log "Read complete element: ~a..." 
                                   (subseq str 0 (min 80 (length str))))
                        (return (parse-full-element str))))
                     ;; Opening tag
                     (t
                      (when (null root-name)
                        (let* ((space-pos (position #\Space tag-content))
                               (gt-pos (position #\> tag-content))
                               (slash-pos (position #\/ tag-content))
                               (end-pos (or space-pos slash-pos (1- gt-pos))))
                          (setf root-name (subseq tag-content 0 end-pos))))
                      (incf depth))))))))

(defun parse-full-element (xml-string)
  "Parse a complete XML element with children from string."
  ;; For now, return a simple xml-element with the raw content
  ;; A full implementation would recursively parse children
  (when (and xml-string (> (length xml-string) 2))
    (let* ((start (position #\< xml-string))
           (first-gt (position #\> xml-string)))
      (when (and start first-gt)
        (let* ((tag-content (subseq xml-string (1+ start) first-gt))
               (space-pos (position #\Space tag-content))
               (slash-pos (position #\/ tag-content))
               (name (cond
                       (space-pos (subseq tag-content 0 space-pos))
                       (slash-pos (subseq tag-content 0 slash-pos))
                       (t tag-content)))
               (attrs (when space-pos
                        (parse-attributes (subseq tag-content space-pos))))
               ;; Extract text content between tags (simple case)
               (end-tag-start (search (format nil "</~a>" name) xml-string))
               (text (when (and end-tag-start (> end-tag-start (1+ first-gt)))
                       (subseq xml-string (1+ first-gt) end-tag-start)))
               ;; Find child elements
               (children (parse-child-elements xml-string (1+ first-gt) end-tag-start)))
          (make-xml-element name 
                            :attributes attrs 
                            :text (when (and text (not children)) text)
                            :children children))))))

(defun parse-child-elements (xml-string start end)
  "Parse child elements from XML string between start and end positions."
  (when (and start end (< start end))
    (let ((children nil)
          (pos start))
      (loop while (< pos end)
            for tag-start = (position #\< xml-string :start pos)
            while (and tag-start (< tag-start end))
            do (let ((tag-end (position #\> xml-string :start tag-start)))
                 (when tag-end
                   (let* ((tag-content (subseq xml-string (1+ tag-start) tag-end))
                          (is-closing (and (> (length tag-content) 0)
                                           (char= (char tag-content 0) #\/)))
                          (is-self-closing (and (> (length tag-content) 0)
                                                (char= (char tag-content (1- (length tag-content))) #\/))))
                     (cond
                       (is-closing
                        (setf pos (1+ tag-end)))
                       (is-self-closing
                        (let* ((space-pos (position #\Space tag-content))
                               (name (subseq tag-content 0 (or space-pos (1- (length tag-content)))))
                               (attrs (when space-pos (parse-attributes (subseq tag-content space-pos)))))
                          (push (make-xml-element name :attributes attrs) children))
                        (setf pos (1+ tag-end)))
                       (t
                        ;; Opening tag - find matching close
                        (let* ((space-pos (position #\Space tag-content))
                               (name (subseq tag-content 0 (or space-pos (length tag-content))))
                               (close-tag (format nil "</~a>" name))
                               (close-pos (search close-tag xml-string :start2 tag-end)))
                          (when close-pos
                            (let ((child-xml (subseq xml-string tag-start (+ close-pos (length close-tag)))))
                              (push (parse-full-element child-xml) children)))
                          (setf pos (if close-pos (+ close-pos (length close-tag)) (1+ tag-end))))))))))
      (nreverse children))))

(defun parse-simple-element (xml-string)
  "Parse a simple XML element from string. Returns xml-element or nil."
  ;; Very basic parser - extracts name and attributes
  (when (and xml-string (> (length xml-string) 2))
    (let* ((start (position #\< xml-string))
           (end (position #\> xml-string :from-end t)))
      (when (and start end)
        (let* ((content (subseq xml-string (1+ start) end))
               (space-pos (position #\Space content))
               (name (if space-pos
                         (subseq content 0 space-pos)
                         (string-right-trim "/" content)))
               (attrs (when space-pos
                        (parse-attributes (subseq content space-pos)))))
          (make-xml-element name :attributes attrs))))))

(defun parse-attributes (attr-string)
  "Parse attributes from a string like \" foo='bar' baz='qux'\"."
  (let ((attrs nil)
        (pos 0)
        (len (length attr-string)))
    (loop while (< pos len)
          for eq-pos = (position #\= attr-string :start pos)
          while eq-pos
          do (let* ((name-start (position-if-not #'whitespace-char-p attr-string :start pos))
                    (name (when name-start (subseq attr-string name-start eq-pos)))
                    (quote-char (char attr-string (1+ eq-pos)))
                    (val-start (+ eq-pos 2))
                    (val-end (position quote-char attr-string :start val-start)))
               (when (and name val-end)
                 (push (cons name (subseq attr-string val-start val-end)) attrs))
               (setf pos (if val-end (1+ val-end) len))))
    (nreverse attrs)))

(defun whitespace-char-p (c)
  "Check if character is whitespace."
  (member c '(#\Space #\Tab #\Newline #\Return)))
