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

(defun process-slash-command (text st ly ui engine)
  "Process a /command. Returns T if handled, NIL otherwise."
  (let* ((parts (str:split " " text :limit 2))
         (cmd (string-downcase (first parts)))
         (args (second parts))
         (pane (focused-chat-pane ly)))
    (cond
      ;; /msg JID [message] - open chat with JID and optionally send message
      ((string= cmd "/msg")
       (when args
         (let* ((msg-parts (str:split " " args :limit 2))
                (jid (first msg-parts))
                (body (second msg-parts)))
           (state-ensure-buffer st jid :title jid)
           (open-buffer! st jid)
           (touch-open-buffer! st jid)
           (setf (pane-buffer-id ly pane) jid)
           (mark-visible-read! st jid)
           (when body
             (let ((cmd (make-instance 'send-message :to jid :body body)))
               (execute cmd st ly engine)))))
       (input-add-to-history ui text)
       (input-clear ui)
       t)
      ;; /help - show available commands
      ((string= cmd "/help")
       (let ((buf (state-ensure-buffer st :system :title "System")))
         (dolist (line '("Available commands:"
                         "  /msg JID [message]  - Open chat (and send message)"
                         "  /add JID [name]     - Add contact to roster"
                         "  /remove JID         - Remove contact from roster"
                         "  /join ROOM [nick]   - Join a MUC room"
                         "  /leave [ROOM]       - Leave MUC (current or specified)"
                         "  /omemo JID          - Init OMEMO session with JID"
                         "  /quit               - Quit CLabber"
                         "  /help               - Show this help"
                         "Keybindings:"
                         "  Tab        - Nick/JID completion in chat"
                         "  Ctrl+O     - Cycle focus (roster/chat)"
                         "  j/k ↑/↓   - Navigate roster (when focused)"
                         "  Enter      - Open roster selection / Send message"
                         "  Ctrl+N/P   - Next/prev roster item"
                         "  Ctrl+W     - Toggle split pane"
                         "  Ctrl+K     - Close current buffer"
                         "  Ctrl+Q     - Quit"
                         "  Alt+1-9    - Jump to buffer"))
           (buffer-add-line buf line))
         (setf (pane-buffer-id ly pane) :system))
       (input-add-to-history ui text)
       (input-clear ui)
       t)
      ;; /add JID [name] - add contact to roster
      ((string= cmd "/add")
       (let ((buf (state-ensure-buffer st :system :title "System")))
         (if args
             (let* ((add-parts (str:split " " args :limit 2))
                    (jid (first add-parts))
                    (name (second add-parts)))
               (handler-case
                   (progn
                     (engine-add-contact engine jid name)
                     (buffer-add-line buf (format nil "Added ~a to roster, subscription request sent" jid)))
                 (error (e)
                   (buffer-add-line buf (format nil "Error adding contact: ~a" e)))))
             (buffer-add-line buf "Usage: /add JID [nickname]")))
       (input-add-to-history ui text)
       (input-clear ui)
       t)
      ;; /remove JID - remove contact from roster
      ((string= cmd "/remove")
       (let ((buf (state-ensure-buffer st :system :title "System")))
         (if args
             (handler-case
                 (progn
                   (engine-remove-contact engine args)
                   (buffer-add-line buf (format nil "Removed ~a from roster" args)))
               (error (e)
                 (buffer-add-line buf (format nil "Error removing contact: ~a" e))))
             (buffer-add-line buf "Usage: /remove JID")))
       (input-add-to-history ui text)
       (input-clear ui)
       t)
      ;; /join ROOM [nick] - join a MUC room
      ((string= cmd "/join")
       (let ((buf (state-ensure-buffer st :system :title "System")))
         (if args
             (let* ((join-parts (str:split " " args :limit 2))
                    (room-jid (first join-parts))
                    (nick (second join-parts)))
               (handler-case
                   (progn
                     (engine-join-muc engine room-jid nick)
                     ;; Add to roster as MUC
                     (let ((existing (find room-jid (state-roster st)
                                          :key #'roster-jid :test #'equal)))
                       (unless existing
                         (push (make-instance 'roster-item
                                              :jid room-jid
                                              :name room-jid
                                              :presence "muc")
                               (state-roster st))))
                     ;; Open buffer for the room
                     (state-ensure-buffer st room-jid :title room-jid)
                     (open-buffer! st room-jid)
                     (touch-open-buffer! st room-jid)
                     (setf (pane-buffer-id ly pane) room-jid)
                     (buffer-add-line buf (format nil "Joining ~a..." room-jid)))
                 (error (e)
                   (buffer-add-line buf (format nil "Error joining MUC: ~a" e)))))
             (buffer-add-line buf "Usage: /join room@conference.server [nickname]")))
       (input-add-to-history ui text)
       (input-clear ui)
       t)
      ;; /leave [ROOM] - leave a MUC room (current buffer or specified)
      ((string= cmd "/leave")
       (let* ((buf (state-ensure-buffer st :system :title "System"))
              (room-jid (or args (pane-buffer-id ly pane))))
         (if (and room-jid (not (eql room-jid :system)))
             (handler-case
                 (progn
                   (when (engine-connection engine)
                     (xmpp-leave-muc (engine-connection engine) room-jid))
                   (buffer-add-line buf (format nil "Left ~a" room-jid)))
               (error (e)
                 (buffer-add-line buf (format nil "Error leaving MUC: ~a" e))))
             (buffer-add-line buf "Usage: /leave [room@conference.server]")))
       (input-add-to-history ui text)
       (input-clear ui)
       t)
      ;; /quit - quit the application
      ((string= cmd "/quit")
       (setf (state-running-p st) nil)
       (input-clear ui)
       t)
      ;; /omemo JID - establish OMEMO session
      ((string= cmd "/omemo")
       (let ((buf (state-ensure-buffer st :system :title "System")))
         (if args
             (progn
               (buffer-add-line buf (format nil "Fetching OMEMO keys for ~a..." args))
               (handler-case
                   (progn
                     (engine-fetch-omemo-keys engine args)
                     (buffer-add-line buf (format nil "OMEMO key fetch initiated for ~a" args)))
                 (error (e)
                   (buffer-add-line buf (format nil "OMEMO error: ~a" e)))))
             (buffer-add-line buf "Usage: /omemo JID")))
       (input-add-to-history ui text)
       (input-clear ui)
       t)
      (t nil))))

(defun process-input-send (st ly ui engine)
  "Process Enter key in chat pane - send message."
  (let* ((text (ui-input-text ui))
         (pane (focused-chat-pane ly))
         (buf-id (pane-buffer-id ly pane))
         (roster-item (when buf-id
                        (find buf-id (state-roster st) 
                              :key #'roster-jid :test #'equal)))
         (is-muc (and roster-item (string= (roster-presence roster-item) "muc"))))
    (when (> (length text) 0)
      ;; Check for slash commands first
      (when (and (> (length text) 0) (char= (char text 0) #\/))
        (when (process-slash-command text st ly ui engine)
          (return-from process-input-send)))
      ;; Normal message send
      (if (not (eql buf-id :system))
          (progn
            (let ((cmd (make-instance 'send-message :to buf-id :body text)))
              (execute cmd st ly engine))
            ;; Send active state after message (XEP-0085)
            (engine-send-chat-state engine buf-id "active" 
                                    :type (if is-muc "groupchat" "chat"))
            (input-add-to-history ui text)
            (input-clear ui))
          ;; Debug: log why we didn't send
          (state-log st (format nil "Not sending: text=~s buf=~a" text buf-id))))))

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
      ;; Disable XON/XOFF flow control so Ctrl+Q reaches the app
      (de.anvi.ncurses:raw)

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
