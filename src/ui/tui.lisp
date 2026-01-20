;;;; tui.lisp - Main TUI loop for CLabber

(in-package #:clabber)

(defun render-all (scr frames st ly ui widgets)
  "Render all widgets to screen."
  (de.anvi.croatoan:clear scr)
  (dolist (entry widgets)
    (destructuring-bind (key . widget) entry
      (let ((rect (getf frames key)))
        (when rect
          (render widget scr rect st ly ui)))))
  (de.anvi.croatoan:refresh scr))

(defun process-input-send (st ly ui engine)
  "Process Enter key in chat pane - send message."
  (let* ((text (ui-input-text ui))
         (pane (focused-chat-pane ly))
         (buf-id (pane-buffer-id ly pane)))
    (if (and (> (length text) 0)
             (not (eql buf-id :system)))
        (progn
          (let ((cmd (make-instance 'send-message :to buf-id :body text)))
            (execute cmd st ly engine))
          (input-add-to-history ui text)
          (input-clear ui))
        ;; Debug: log why we didn't send
        (state-log st (format nil "Not sending: text=~s buf=~a" text buf-id)))))

(defun ui-run (engine)
  "Run the main UI loop."
  (let ((st (make-instance 'app-state))
        (ly (make-instance 'layout))
        (ui (make-instance 'ui-state))
        (widgets nil))

    (setf (layout-roster-width ly) (config-roster-width *config*))
    (setf (state-auto-open-on-message-p st) (config-auto-open-on-message-p *config*))

    (setf widgets (list (cons :roster (make-instance 'roster-widget))
                        (cons :bufferbar (make-instance 'bufferbar-widget))
                        (cons :chat-a (make-instance 'chat-widget :pane :chat-a))
                        (cons :chat-b (make-instance 'chat-widget :pane :chat-b))
                        (cons :status (make-instance 'status-widget))
                        (cons :input (make-instance 'input-widget))))

    (de.anvi.croatoan:with-screen (scr :input-echoing nil :cursor-visible nil)
      (setf (de.anvi.croatoan:input-blocking scr) nil)

      (loop while (state-running-p st) do
        ;; 1) Apply engine events
        (dolist (evt (q-drain (engine-queue engine)))
          (setf st (apply-event evt st ly)))

        ;; 2) Read and process key
        (let ((key (de.anvi.croatoan:get-char scr)))
          (when key
            ;; Global keymap -> commands
            (let ((cmds (key->commands key st ly ui)))
              (when cmds
                (multiple-value-setq (st ly) (execute-commands cmds st ly engine))))

            ;; Enter in chat pane sends message (10 = LF, 13 = CR)
            (when (and (or (eql key #\Newline) (eql key #\Return)
                           (eql key 10) (eql key 13))
                       (member (layout-focused-pane ly) '(:chat-a :chat-b)))
              (process-input-send st ly ui engine))

            ;; Input widget collects typed chars only when focus is on chat pane
            (when (member (layout-focused-pane ly) '(:chat-a :chat-b))
              (handle-input (cdr (assoc :input widgets)) key st ly ui))))

        ;; 3) Render
        (let ((frames (compute-frames scr ly)))
          (render-all scr frames st ly ui widgets))

        (sleep 0.03)))))
