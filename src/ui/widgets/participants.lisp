;;;; participants.lisp - MUC participant list widget for CLabber

(in-package #:clabber)

(defclass participants-widget (widget)
  ()
  (:documentation "Widget displaying MUC room participants."))

(defmethod render ((w participants-widget) scr rect st ly ui)
  (declare (ignore w ui))
  (let* ((x (rx rect))
         (y (ry rect))
         (width (rw rect))
         (height (rh rect))
         (theme (current-theme))
         ;; Get the focused chat pane's buffer
         (pane (focused-chat-pane ly))
         (buf-id (pane-buffer-id ly pane))
         (buf (when buf-id (find-buffer st buf-id)))
         ;; Check if this is a MUC room
         (roster-item (when buf-id
                        (find buf-id (state-roster st) 
                              :key #'roster-jid :test #'equal)))
         (is-muc (and roster-item (string= (roster-presence roster-item) "muc")))
         ;; Selected participant index
         (selected-idx (layout-participant-index ly)))
    ;; Draw border
    (de.anvi.croatoan:move scr y x)
    (de.anvi.croatoan:add-string scr (make-string width :initial-element #\-))
    
    ;; Title
    (let ((title (if is-muc "Participants" "---")))
      (de.anvi.croatoan:move scr y (+ x 1))
      (de.anvi.croatoan:add-string scr title))
    
    ;; Draw participants if MUC
    (when (and is-muc buf)
      (let ((participants (buffer-participants buf))
            (row 1)
            (max-rows (1- height))
            (idx 0))
        ;; Sort participants alphabetically
        (let ((nicks nil))
          (maphash (lambda (nick show)
                     (declare (ignore show))
                     (push nick nicks))
                   participants)
          (setf nicks (sort nicks #'string-lessp))
          ;; Show count in title
          (de.anvi.croatoan:move scr y (+ x 1))
          (de.anvi.croatoan:add-string scr (format nil "Participants (~d)" (length nicks)))
          ;; Display each participant
          (dolist (nick nicks)
            (when (< row max-rows)
              (let* ((show (gethash nick participants))
                     (color (theme-presence-color theme show))
                     (is-selected (= idx selected-idx))
                     (marker (if is-selected ">" " "))
                     (display-nick (if (> (length nick) (- width 3))
                                       (subseq nick 0 (- width 3))
                                       nick)))
                (de.anvi.croatoan:move scr (+ y row) x)
                (de.anvi.croatoan:add-string scr marker)
                (if is-selected
                    ;; Highlight selected participant
                    (de.anvi.croatoan:add-string scr display-nick 
                                                 :color-pair (list :black color))
                    ;; Normal display
                    (de.anvi.croatoan:add-string scr display-nick 
                                                 :color-pair (list color nil))))
              (incf row)
              (incf idx))))))))
