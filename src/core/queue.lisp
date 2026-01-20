;;;; queue.lisp - Thread-safe event queue for CLabber

(in-package #:clabber)

(defclass event-queue ()
  ((lock  :initform (bt:make-lock "clabber-event-queue") :reader q-lock)
   (items :initform '() :accessor q-items)))

(defun make-event-queue ()
  "Create a new event queue."
  (make-instance 'event-queue))

(defun q-push (q evt)
  "Push event EVT onto queue Q."
  (bt:with-lock-held ((q-lock q))
    (setf (q-items q) (nconc (q-items q) (list evt)))))

(defun q-drain (q)
  "Drain and return all events from queue Q."
  (bt:with-lock-held ((q-lock q))
    (prog1 (q-items q)
      (setf (q-items q) '()))))
