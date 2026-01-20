;;; CLabber User Theme (CLOS format)
;;; Copy this to ~/.config/clabber/themes/theme.lisp and edit as needed.
;;; Edit this file to customize colors - no rebuild needed.
;;; Available colors: :red :green :yellow :blue :magenta :cyan :white :black
;;;                   :lime :maroon :olive :navy :purple :teal

(in-package #:clabber)

;;; Custom theme example - Tokyo Night with rounded corners
(defclass my-custom-theme (rounded-theme)
  ()
  (:default-initargs
   ;; Nick colors for chat messages (hashed by nick)
   :nick-colors '(:magenta :cyan :green :yellow :red :white)
   ;; Presence indicator colors
   :presence-available :green
   :presence-away :yellow
   :presence-dnd :red
   :presence-xa :magenta
   :presence-offline :white
   ;; Message level colors
   :system-color :blue
   :error-color :red
   :notice-color :cyan
   :highlight-color :magenta
   ;; UI element colors
   :status-fg :cyan
   :border-fg :blue
   :active-buffer-fg :magenta
   :unread-fg :cyan
   :mention-fg :magenta
   :roster-selected-fg :magenta
   ;; Box drawing characters (inherited from rounded-theme)
   ;; :box-tl #\╭  :box-tr #\╮  :box-bl #\╰  :box-br #\╯
   ;; Prompt
   :input-prompt "> "))

;; Activate the theme
(set-theme 'my-custom-theme)

;;; Alternative: Use a built-in theme
;;; Available themes: dark, light, solarized, minimal, ascii, rounded, tokyo-night
;; (set-theme 'dark-theme)
;; (set-theme 'ascii-theme)  ; For terminals without Unicode support
