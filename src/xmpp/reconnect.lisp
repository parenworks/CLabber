;;;; reconnect.lisp - Reconnection policy for CLabber

(in-package #:clabber)

(defclass reconnect-policy ()
  ((enabled :initform t :accessor reconnect-enabled-p)
   (max-attempts :initform 10 :accessor reconnect-max-attempts)
   (base-delay :initform 1.0 :accessor reconnect-base-delay)
   (max-delay :initform 300.0 :accessor reconnect-max-delay)
   (current-attempt :initform 0 :accessor reconnect-current-attempt)
   (last-attempt-time :initform nil :accessor reconnect-last-attempt-time)))

(defun make-reconnect-policy (&key (enabled t) (max-attempts 10)
                                   (base-delay 1.0) (max-delay 300.0))
  "Create a new reconnect policy."
  (make-instance 'reconnect-policy
                 :enabled enabled
                 :max-attempts max-attempts
                 :base-delay base-delay
                 :max-delay max-delay))

(defun reconnect-delay (policy)
  "Calculate delay before next reconnection attempt (exponential backoff)."
  (let* ((attempt (reconnect-current-attempt policy))
         (base (reconnect-base-delay policy))
         (max-d (reconnect-max-delay policy))
         (delay (* base (expt 2 attempt))))
    (min delay max-d)))

(defun reconnect-should-retry-p (policy)
  "Return T if we should attempt reconnection."
  (and (reconnect-enabled-p policy)
       (< (reconnect-current-attempt policy)
          (reconnect-max-attempts policy))))

(defun reconnect-record-attempt (policy)
  "Record a reconnection attempt."
  (incf (reconnect-current-attempt policy))
  (setf (reconnect-last-attempt-time policy) (get-universal-time))
  policy)

(defun reconnect-reset (policy)
  "Reset reconnection state after successful connection."
  (setf (reconnect-current-attempt policy) 0)
  (setf (reconnect-last-attempt-time policy) nil)
  policy)

(defun schedule-reconnect (engine policy queue)
  "Schedule a reconnection attempt if policy allows."
  (declare (ignore engine))  ; TODO: Use engine for reconnection
  (when (reconnect-should-retry-p policy)
    (let ((delay (reconnect-delay policy)))
      (reconnect-record-attempt policy)
      (bt:make-thread
       (lambda ()
         (sleep delay)
         (when (reconnect-enabled-p policy)
           ;; TODO: Attempt reconnection via engine
           (q-push queue
                   (make-instance 'xmpp-connecting
                                  :jid "reconnecting..."))))
       :name "clabber-reconnect-thread"))))
