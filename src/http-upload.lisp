(in-package #:clabber)

;;; XEP-0363 HTTP File Upload
;;; Discover upload service, request slot, PUT file, send URL

(defvar *upload-service-jid* nil
  "JID of the HTTP Upload service component (discovered via disco).")

(defvar *pending-upload* nil
  "Plist of pending upload: (:file path :buffer buf :put-url url :get-url url :headers headers)")

;;; ============================================================
;;; Service Discovery for HTTP Upload
;;; ============================================================

(defun xmpp-disco-items (conn jid)
  "Send a disco#items query to JID to discover server components."
  (let* ((query (make-xml-element "query" :namespace clabber.xmpp::+ns-disco-items+))
         (iq (make-iq-stanza "get" :to jid :query query
                             :id (format nil "disco-items-~a" (random 100000)))))
    (xmpp-send conn iq)))

(defun xmpp-disco-info-for-upload (conn jid)
  "Send disco#info to a component JID to check if it supports HTTP Upload."
  (let* ((query (make-xml-element "query" :namespace clabber.xmpp::+ns-disco-info+))
         (iq (make-iq-stanza "get" :to jid :query query
                             :id (format nil "disco-upload-~a" (random 100000)))))
    (xmpp-send conn iq)))

;;; ============================================================
;;; Upload Slot Request (XEP-0363)
;;; ============================================================

(defun xmpp-request-upload-slot (conn filename size &optional content-type)
  "Request an HTTP Upload slot from the upload service."
  (unless *upload-service-jid*
    (error "No HTTP Upload service discovered"))
  (let* ((request (make-xml-element "request"
                    :namespace clabber.xmpp::+ns-http-upload+
                    :attributes `(("filename" . ,filename)
                                  ("size" . ,(princ-to-string size))
                                  ,@(when content-type
                                      `(("content-type" . ,content-type))))))
         (iq (make-iq-stanza "get" :to *upload-service-jid* :query request
                             :id (format nil "upload-slot-~a" (random 100000)))))
    (xmpp-send conn iq)))

;;; ============================================================
;;; HTTP PUT (minimal, using usocket + cl+ssl)
;;; ============================================================

(defun parse-url (url)
  "Parse URL into (scheme host port path). Returns values."
  (let* ((scheme-end (search "://" url))
         (scheme (subseq url 0 scheme-end))
         (rest (subseq url (+ scheme-end 3)))
         (path-start (position #\/ rest))
         (host-port (if path-start (subseq rest 0 path-start) rest))
         (path (if path-start (subseq rest path-start) "/"))
         (colon (position #\: host-port))
         (host (if colon (subseq host-port 0 colon) host-port))
         (port (if colon
                   (parse-integer (subseq host-port (1+ colon)))
                   (if (string= scheme "https") 443 80))))
    (values scheme host port path)))

(defun http-put-file (url file-path content-type &optional extra-headers)
  "Upload FILE-PATH to URL via HTTP PUT. Returns HTTP status code."
  (multiple-value-bind (scheme host port path) (parse-url url)
    (let* ((file-size (with-open-file (f file-path :element-type '(unsigned-byte 8))
                        (file-length f)))
           (use-ssl (string= scheme "https"))
           (socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
           (raw-stream (usocket:socket-stream socket))
           (stream (if use-ssl
                       (cl+ssl:make-ssl-client-stream raw-stream
                         :hostname host
                         :external-format '(:utf-8 :eol-style :crlf))
                       raw-stream)))
      (unwind-protect
          (progn
            ;; Send HTTP PUT request
            (let* ((crlf (coerce (list #\Return #\Linefeed) 'string))
                   (header (with-output-to-string (s)
                             (format s "PUT ~A HTTP/1.1" path) (write-string crlf s)
                             (format s "Host: ~A" host) (write-string crlf s)
                             (format s "Content-Type: ~A" (or content-type "application/octet-stream")) (write-string crlf s)
                             (format s "Content-Length: ~D" file-size) (write-string crlf s)
                             (dolist (h extra-headers)
                               (format s "~A: ~A" (car h) (cdr h)) (write-string crlf s))
                             (write-string crlf s))))
              (let ((header-bytes (babel:string-to-octets header :encoding :utf-8)))
                (write-sequence header-bytes stream)))
            ;; Send file body
            (with-open-file (f file-path :element-type '(unsigned-byte 8))
              (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
                (loop for n = (read-sequence buf f)
                      while (> n 0)
                      do (write-sequence buf stream :end n))))
            (force-output stream)
            ;; Read response status line
            (let ((response-line (read-http-line stream)))
              (debug-log "HTTP Upload response: ~a" response-line)
              ;; Parse status code from "HTTP/1.1 201 Created"
              (let ((space1 (position #\Space response-line)))
                (when space1
                  (let ((space2 (position #\Space response-line :start (1+ space1))))
                    (when space2
                      (parse-integer (subseq response-line (1+ space1) space2))))))))
        (ignore-errors (close stream))
        (ignore-errors (usocket:socket-close socket))))))

(defun read-http-line (stream)
  "Read a line from an HTTP stream (CRLF terminated)."
  (let ((buf (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          while (and byte (not (= byte (char-code #\Newline))))
          do (unless (= byte (char-code #\Return))
               (vector-push-extend (code-char byte) buf)))
    (coerce buf 'string)))

;;; ============================================================
;;; Handle Upload Slot Response
;;; ============================================================

(defun handle-upload-slot-response (app stanza)
  "Handle the IQ result containing PUT and GET URLs for HTTP Upload."
  (let ((query (iq-query stanza)))
    (when (and query (string= (xml-name query) "slot"))
      (let ((put-el (xml-child query "put"))
            (get-el (xml-child query "get")))
        (when (and put-el get-el)
          (let ((put-url (xml-attr put-el "url"))
                (get-url (xml-attr get-el "url"))
                ;; Collect any required headers from <header> children of <put>
                (headers (loop for child in (xml-children put-el)
                               when (and (typep child 'xml-element)
                                         (string= (xml-name child) "header"))
                               collect (cons (xml-attr child "name")
                                             (xml-text child)))))
            (debug-log "Upload slot: PUT=~a GET=~a headers=~a" put-url get-url headers)
            ;; Perform the upload in a background thread
            (when *pending-upload*
              (let ((file-path (getf *pending-upload* :file))
                    (buf (getf *pending-upload* :buffer))
                    (content-type (getf *pending-upload* :content-type)))
                (bt:make-thread
                 (lambda ()
                   (handler-case
                       (let ((status (http-put-file put-url file-path content-type headers)))
                         (debug-log "Upload complete, status=~a" status)
                         (if (and status (< status 300))
                             ;; Success: queue event to send the URL as a message
                             (progn
                               (bt:with-lock-held ((app-event-lock app))
                                 (push (list :upload-complete :url get-url :buffer buf)
                                       (app-events app))))
                             ;; Failure
                             (bt:with-lock-held ((app-event-lock app))
                               (push (list :upload-error
                                           :message (format nil "HTTP ~a" status)
                                           :buffer buf)
                                     (app-events app)))))
                     (error (e)
                       (debug-log "Upload error: ~a" e)
                       (bt:with-lock-held ((app-event-lock app))
                         (push (list :upload-error
                                     :message (format nil "~a" e)
                                     :buffer buf)
                               (app-events app))))))
                 :name "http-upload")
                (setf *pending-upload* nil)))))))))

;;; ============================================================
;;; Guess Content-Type from filename
;;; ============================================================

(defun guess-content-type (filename)
  "Guess MIME type from file extension."
  (let ((ext (let ((dot (position #\. filename :from-end t)))
               (when dot (string-downcase (subseq filename (1+ dot)))))))
    (cond
      ((member ext '("jpg" "jpeg") :test #'string=) "image/jpeg")
      ((string= ext "png") "image/png")
      ((string= ext "gif") "image/gif")
      ((string= ext "webp") "image/webp")
      ((string= ext "svg") "image/svg+xml")
      ((string= ext "mp4") "video/mp4")
      ((string= ext "webm") "video/webm")
      ((string= ext "mp3") "audio/mpeg")
      ((string= ext "ogg") "audio/ogg")
      ((string= ext "pdf") "application/pdf")
      ((string= ext "txt") "text/plain")
      ((string= ext "zip") "application/zip")
      ((string= ext "tar") "application/x-tar")
      ((string= ext "gz") "application/gzip")
      (t "application/octet-stream"))))

;;; ============================================================
;;; 0x0.st Pastebin Integration
;;; ============================================================

(defun 0x0-upload-file (file-path)
  "Upload FILE-PATH to 0x0.st. Returns the URL string on success."
  (let* ((boundary (format nil "----CLabber~A" (random 1000000)))
         (filename (file-namestring file-path))
         (content-type (guess-content-type filename))
         (file-bytes (with-open-file (f file-path :element-type '(unsigned-byte 8))
                       (let ((buf (make-array (file-length f) :element-type '(unsigned-byte 8))))
                         (read-sequence buf f)
                         buf)))
         (crlf (coerce (list #\Return #\Linefeed) 'string))
         ;; Build multipart body
         (body-prefix (babel:string-to-octets
                       (with-output-to-string (s)
                         (format s "--~A" boundary) (write-string crlf s)
                         (format s "Content-Disposition: form-data; name=\"file\"; filename=\"~A\"" filename) (write-string crlf s)
                         (format s "Content-Type: ~A" content-type) (write-string crlf s)
                         (write-string crlf s))
                       :encoding :utf-8))
         (body-suffix (babel:string-to-octets
                       (with-output-to-string (s)
                         (write-string crlf s)
                         (format s "--~A--" boundary) (write-string crlf s))
                       :encoding :utf-8))
         (body-len (+ (length body-prefix) (length file-bytes) (length body-suffix))))
    (0x0-http-post boundary body-prefix file-bytes body-suffix body-len)))

(defun 0x0-upload-text (text &optional (filename "paste.txt"))
  "Upload TEXT string to 0x0.st as a text file. Returns the URL string."
  (let* ((boundary (format nil "----CLabber~A" (random 1000000)))
         (crlf (coerce (list #\Return #\Linefeed) 'string))
         (text-bytes (babel:string-to-octets text :encoding :utf-8))
         (body-prefix (babel:string-to-octets
                       (with-output-to-string (s)
                         (format s "--~A" boundary) (write-string crlf s)
                         (format s "Content-Disposition: form-data; name=\"file\"; filename=\"~A\"" filename) (write-string crlf s)
                         (format s "Content-Type: text/plain; charset=utf-8") (write-string crlf s)
                         (write-string crlf s))
                       :encoding :utf-8))
         (body-suffix (babel:string-to-octets
                       (with-output-to-string (s)
                         (write-string crlf s)
                         (format s "--~A--" boundary) (write-string crlf s))
                       :encoding :utf-8))
         (body-len (+ (length body-prefix) (length text-bytes) (length body-suffix))))
    (0x0-http-post boundary body-prefix text-bytes body-suffix body-len)))

(defun 0x0-http-post (boundary prefix file-bytes suffix body-len)
  "POST multipart form data to 0x0.st. Returns response body (URL)."
  (let* ((host "0x0.st")
         (port 443)
         (socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
         (raw-stream (usocket:socket-stream socket))
         (stream (cl+ssl:make-ssl-client-stream raw-stream
                   :hostname host
                   :external-format '(:utf-8 :eol-style :crlf))))
    (unwind-protect
        (progn
          (let* ((crlf (coerce (list #\Return #\Linefeed) 'string))
                 (header (with-output-to-string (s)
                           (format s "POST / HTTP/1.1") (write-string crlf s)
                           (format s "Host: ~A" host) (write-string crlf s)
                           (format s "User-Agent: CLabber/2.0") (write-string crlf s)
                           (format s "Content-Type: multipart/form-data; boundary=~A" boundary) (write-string crlf s)
                           (format s "Content-Length: ~D" body-len) (write-string crlf s)
                           (format s "Connection: close") (write-string crlf s)
                           (write-string crlf s)))
                 (header-bytes (babel:string-to-octets header :encoding :utf-8)))
            (write-sequence header-bytes stream)
            (write-sequence prefix stream)
            (write-sequence file-bytes stream)
            (write-sequence suffix stream)
            (force-output stream)
            ;; Read response
            (let ((status-line (read-http-line stream))
                  (url nil))
              (debug-log "0x0.st response: ~a" status-line)
              ;; Skip headers until blank line
              (loop for line = (read-http-line stream)
                    while (> (length line) 0))
              ;; Read body (the URL)
              (setf url (read-http-line stream))
              (when (and url (> (length url) 0))
                (string-trim '(#\Space #\Return #\Linefeed) url)))))
      (ignore-errors (close stream))
      (ignore-errors (usocket:socket-close socket)))))
