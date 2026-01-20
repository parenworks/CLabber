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
         (buf-id (pane-buffer-id ly pane))
         (roster-item (when buf-id
                        (find buf-id (state-roster st) 
                              :key #'roster-jid :test #'equal)))
         (is-muc (and roster-item (string= (roster-presence roster-item) "muc"))))
    (if (and (> (length text) 0)
             (not (eql buf-id :system)))
        (progn
          (let ((cmd (make-instance 'send-message :to buf-id :body text)))
            (execute cmd st ly engine))
          ;; Send active state after message (XEP-0085)
          (engine-send-chat-state engine buf-id "active" 
                                  :type (if is-muc "groupchat" "chat"))
          (input-add-to-history ui text)
          (input-clear ui))
        ;; Debug: log why we didn't send
        (state-log st (format nil "Not sending: text=~s buf=~a" text buf-id)))))

(defun read-key-with-escape (scr)
  "Read a key, handling escape sequences for Alt+key combinations.
   Returns either a single key or (ESC . char) for Alt combinations."
  (let ((key (de.anvi.croatoan:get-char scr)))
    (when (and key (not (eql key -1)))
      ;; Debug: log actual key presses (not -1 which is "no key")
      (debug-log "Key received: ~s (type: ~a)" key (type-of key))
      ;; Check for meta bit (Alt sets 8th bit, so values 128-255)
      (when (and (integerp key) (>= key 128) (<= key 255))
        (let ((base-char (code-char (- key 128))))
          (debug-log "Meta key detected: ~s -> ~s" key base-char)
          (return-from read-key-with-escape (cons 27 base-char))))
      ;; ESC can be integer 27 or character #\Escape
      (when (or (eql key 27) (eql key #\Escape))
        ;; Wait briefly for following character (Alt+key sends ESC then char quickly)
        (sleep 0.01)
        (let ((next (de.anvi.croatoan:get-char scr)))
          (when (and next (not (eql next -1)))
            (debug-log "ESC sequence: 27 + ~s" next)
            (return-from read-key-with-escape (cons 27 next)))))
      (return-from read-key-with-escape key))
    nil))

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
                        (cons :input (make-instance 'input-widget))
                        (cons :participants (make-instance 'participants-widget))))

    (de.anvi.croatoan:with-screen (scr :input-echoing nil :cursor-visible nil)
      (setf (de.anvi.croatoan:input-blocking scr) nil)

      (loop while (state-running-p st) do
        ;; 1) Apply engine events
        (dolist (evt (q-drain (engine-queue engine)))
          (setf st (apply-event evt st ly)))

        ;; 2) Read and process key (with Alt+key handling)
        (let ((key (read-key-with-escape scr)))
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
              (handle-input (cdr (assoc :input widgets)) key st ly ui)
              ;; Send composing state if user started typing
              (when (ui-composing-p ui)
                (setf (ui-composing-p ui) nil)
                (let* ((pane (focused-chat-pane ly))
                       (buf-id (pane-buffer-id ly pane))
                       (roster-item (when buf-id
                                      (find buf-id (state-roster st) 
                                            :key #'roster-jid :test #'equal)))
                       (is-muc (and roster-item (string= (roster-presence roster-item) "muc"))))
                  (when buf-id
                    (engine-send-chat-state engine buf-id "composing" 
                                            :type (if is-muc "groupchat" "chat"))))))))

        ;; 3) Render - check if current buffer is MUC to show participants
        (let* ((pane (focused-chat-pane ly))
               (buf-id (pane-buffer-id ly pane))
               (roster-item (when buf-id
                              (find buf-id (state-roster st) 
                                    :key #'roster-jid :test #'equal)))
               (is-muc (and roster-item (string= (roster-presence roster-item) "muc")))
               (frames (compute-frames scr ly :show-participants is-muc)))
          (render-all scr frames st ly ui widgets))

        (sleep 0.03)))))
