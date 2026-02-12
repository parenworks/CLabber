;;;; classes.lisp - UI classes for CLabber

(in-package #:clabber)

(defclass ui-state ()
  ((input-text :initform "" :accessor ui-input-text)
   (input-cursor :initform 0 :accessor ui-input-cursor)
   (input-history :initform '() :accessor ui-input-history)
   (history-index :initform -1 :accessor ui-history-index)
   (composing-p :initform nil :accessor ui-composing-p
                :documentation "T when user started typing and composing state should be sent")
   (tab-candidates :initform nil :accessor ui-tab-candidates
                   :documentation "List of tab-completion candidates")
   (tab-index :initform 0 :accessor ui-tab-index
              :documentation "Current index into tab-completion candidates")
   (tab-prefix :initform "" :accessor ui-tab-prefix
               :documentation "The prefix text before the word being completed")
   (tab-word :initform "" :accessor ui-tab-word
             :documentation "The partial word being completed")))

(defclass widget () ())

(defgeneric render (w scr rect st ly ui)
  (:documentation "Render widget W to screen SCR within RECT."))

(defgeneric handle-input (w key st ly ui)
  (:documentation "Handle KEY input for widget W. Return list of commands or NIL."))
