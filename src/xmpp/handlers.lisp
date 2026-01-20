;;;; handlers.lisp - XMPP stanza handlers for CLabber

(in-package #:clabber)

;; These handlers will be wired up to cl-ngxmpp-client callbacks.
;; Each handler receives the raw stanza and pushes appropriate events
;; to the engine's event queue.

(defun handle-message-stanza (engine stanza)
  "Handle incoming message stanza."
  (declare (ignore stanza))
  ;; TODO: Parse stanza, extract from/body
  ;; (let ((from (xmpp-stanza-from stanza))
  ;;       (body (xmpp-stanza-body stanza)))
  ;;   (when body
  ;;     (q-push (engine-queue engine)
  ;;             (make-instance 'xmpp-message :from from :body body))))
  (declare (ignore engine))
  nil)

(defun handle-presence-stanza (engine stanza)
  "Handle incoming presence stanza."
  (declare (ignore stanza))
  ;; TODO: Parse stanza, extract jid/show/status
  ;; (let ((jid (xmpp-stanza-from stanza))
  ;;       (show (xmpp-stanza-show stanza))
  ;;       (status (xmpp-stanza-status stanza)))
  ;;   (q-push (engine-queue engine)
  ;;           (make-instance 'xmpp-presence :jid jid :show show :status status)))
  (declare (ignore engine))
  nil)

(defun handle-roster-result (engine items)
  "Handle roster query result."
  ;; TODO: Convert raw roster items to roster-item objects
  ;; (let ((roster-items
  ;;         (mapcar (lambda (item)
  ;;                   (make-instance 'roster-item
  ;;                                  :jid (getf item :jid)
  ;;                                  :name (getf item :name)))
  ;;                 items)))
  ;;   (q-push (engine-queue engine)
  ;;           (make-instance 'roster-update :items roster-items)))
  (declare (ignore engine items))
  nil)

(defun handle-iq-stanza (engine stanza)
  "Handle incoming IQ stanza."
  (declare (ignore engine stanza))
  ;; TODO: Route to appropriate handler based on IQ type/namespace
  nil)

(defun handle-error-stanza (engine stanza)
  "Handle XMPP error stanza."
  (declare (ignore stanza))
  ;; TODO: Parse error, push error-event
  ;; (q-push (engine-queue engine)
  ;;         (make-instance 'error-event :where :xmpp :condition "XMPP error"))
  (declare (ignore engine))
  nil)
