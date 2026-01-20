;;;; log.lisp - Logging utilities for CLabber

(in-package #:clabber)

(defparameter *log-level* :info
  "Current log level: :debug, :info, :warn, :error")

(defparameter *log-file* nil
  "Optional log file path for debug logging.")

(defun log-level-value (level)
  "Return numeric value for log level."
  (case level
    (:debug 0)
    (:info 1)
    (:warn 2)
    (:error 3)
    (t 1)))

(defun should-log-p (level)
  "Return T if LEVEL should be logged given current *log-level*."
  (>= (log-level-value level) (log-level-value *log-level*)))

(defun format-log-message (level format-string args)
  "Format a log message with timestamp and level."
  (format nil "[~a] [~a] ~a"
          (local-time:format-timestring nil (local-time:now)
                                        :format '(:hour ":" :min ":" :sec))
          (string-upcase (symbol-name level))
          (apply #'format nil format-string args)))

(defun log-message (level format-string &rest args)
  "Log a message at LEVEL."
  (when (should-log-p level)
    (let ((msg (format-log-message level format-string args)))
      (when *log-file*
        (with-open-file (out *log-file* :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :create)
          (write-line msg out)))
      msg)))

(defun log-debug (format-string &rest args)
  "Log a debug message."
  (apply #'log-message :debug format-string args))

(defun log-info (format-string &rest args)
  "Log an info message."
  (apply #'log-message :info format-string args))

(defun log-warn (format-string &rest args)
  "Log a warning message."
  (apply #'log-message :warn format-string args))

(defun log-error (format-string &rest args)
  "Log an error message."
  (apply #'log-message :error format-string args))
