(in-package #:clabber.theme)

;;; Theme System for CLabber UI
;;; Tokyo Night inspired color scheme with CLOS-based theming

;;; Base theme class - defines all customizable visual elements

(defclass base-theme ()
  (;; Nick colors for chat messages
   (nick-colors :initarg :nick-colors
                :accessor theme-nick-colors
                :initform nil)
   ;; Background and foreground
   (bg :initarg :bg :accessor theme-bg :initform nil)
   (fg :initarg :fg :accessor theme-fg :initform nil)
   ;; Border colors
   (border-active :initarg :border-active :accessor theme-border-active :initform nil)
   (border-inactive :initarg :border-inactive :accessor theme-border-inactive :initform nil)
   ;; Indicator colors
   (unread-indicator :initarg :unread-indicator :accessor theme-unread-indicator :initform nil)
   (mention-indicator :initarg :mention-indicator :accessor theme-mention-indicator :initform nil)
   (omemo-lock :initarg :omemo-lock :accessor theme-omemo-lock :initform nil)
   (timestamp-color :initarg :timestamp :accessor theme-timestamp :initform nil)
   ;; Message level colors
   (join-color :initarg :join-color :accessor theme-join-color :initform nil)
   (part-color :initarg :part-color :accessor theme-part-color :initform nil)
   (error-color :initarg :error-color :accessor theme-error-color :initform nil)
   (system-color :initarg :system-color :accessor theme-system-color :initform nil)
   (presence-color :initarg :presence-color :accessor theme-presence-color :initform nil)
   ;; Input prompt
   (input-prompt :initarg :input-prompt :accessor theme-input-prompt :initform "> ")
   ;; Box drawing characters
   (box-h :initarg :box-h :accessor theme-box-h :initform #\─)
   (box-v :initarg :box-v :accessor theme-box-v :initform #\│)
   (box-tl :initarg :box-tl :accessor theme-box-tl :initform #\┌)
   (box-tr :initarg :box-tr :accessor theme-box-tr :initform #\┐)
   (box-bl :initarg :box-bl :accessor theme-box-bl :initform #\└)
   (box-br :initarg :box-br :accessor theme-box-br :initform #\┘)
   (box-t-down :initarg :box-t-down :accessor theme-box-t-down :initform #\┬)
   (box-t-up :initarg :box-t-up :accessor theme-box-t-up :initform #\┴)
   (box-t-right :initarg :box-t-right :accessor theme-box-t-right :initform #\├)
   (box-t-left :initarg :box-t-left :accessor theme-box-t-left :initform #\┤)
   (box-cross :initarg :box-cross :accessor theme-box-cross :initform #\┼))
  (:documentation "Base theme class defining all visual elements."))

;;; Generic functions for theme operations

(defgeneric theme-level-color (theme level)
  (:documentation "Get the color for a message level from the theme."))

(defgeneric theme-nick-color (theme nick)
  (:documentation "Get a consistent color for a nick from the theme."))

(defmethod theme-level-color ((theme base-theme) level)
  (case level
    (:join (theme-join-color theme))
    (:part (theme-part-color theme))
    (:error (theme-error-color theme))
    (:system (theme-system-color theme))
    (:presence (theme-presence-color theme))
    (otherwise nil)))

(defmethod theme-nick-color ((theme base-theme) nick)
  "Hash a nick to a consistent color from the theme's palette."
  (let* ((colors (theme-nick-colors theme))
         (hash (reduce #'+ (map 'list #'char-code nick))))
    (when colors
      (nth (mod hash (length colors)) colors))))

;;; Tokyo Night theme - default

(defclass tokyo-night-theme (base-theme)
  ()
  (:default-initargs
   ;; Tokyo Night color palette using RGB
   :bg (make-rgb-color #x1a #x1b #x26)          ; #1a1b26 - deep navy
   :fg (make-rgb-color #xc0 #xca #xf5)          ; #c0caf5 - soft white
   :border-active (make-rgb-color #x7a #xa2 #xf7)   ; #7aa2f7 - bright blue
   :border-inactive (make-rgb-color #x3b #x42 #x61) ; #3b4261 - muted gray
   :unread-indicator (make-rgb-color #xff #x9e #x64) ; #ff9e64 - orange
   :mention-indicator (make-rgb-color #xf7 #x76 #x8e) ; #f7768e - pink/red
   :omemo-lock (make-rgb-color #x9e #xce #x6a)  ; #9ece6a - green
   :timestamp (make-rgb-color #x56 #x5f #x89)   ; #565f89 - dim gray
   :join-color (make-rgb-color #x9e #xce #x6a)  ; #9ece6a - green
   :part-color (make-rgb-color #xe0 #xaf #x68)  ; #e0af68 - amber
   :error-color (make-rgb-color #xf7 #x76 #x8e) ; #f7768e - pink/red
   :system-color (make-rgb-color #x7a #xa2 #xf7) ; #7aa2f7 - blue
   :presence-color (make-rgb-color #xbb #x9a #xf7) ; #bb9af7 - purple
   :nick-colors (list
                 (make-rgb-color #x7a #xa2 #xf7)  ; blue
                 (make-rgb-color #x9e #xce #x6a)  ; green
                 (make-rgb-color #xe0 #xaf #x68)  ; amber
                 (make-rgb-color #xf7 #x76 #x8e)  ; pink
                 (make-rgb-color #xbb #x9a #xf7)  ; purple
                 (make-rgb-color #x7d #xcf #xff)  ; light blue
                 (make-rgb-color #xff #x9e #x64)  ; orange
                 (make-rgb-color #x2a #xc3 #xde)  ; teal
                 (make-rgb-color #xc0 #xca #xf5)  ; white
                 (make-rgb-color #x73 #xda #xca)  ; mint
                 (make-rgb-color #xb4 #xf9 #xf8)  ; cyan
                 (make-rgb-color #xff #x75 #x7f))  ; coral
   :input-prompt "❯ "))

;;; Current theme

(defvar *current-theme* (make-instance 'tokyo-night-theme)
  "The active theme instance")

(defun current-theme ()
  "Get the current theme"
  *current-theme*)
