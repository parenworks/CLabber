;;;; keymap.lisp - Key bindings for CLabber (CLOS-based)

(in-package #:clabber)

;;; ============================================================
;;; Key Constants
;;; ============================================================

(defparameter +key-ctrl-a+ 1)
(defparameter +key-ctrl-b+ 2)
(defparameter +key-ctrl-e+ 5)
(defparameter +key-ctrl-f+ 6)
(defparameter +key-ctrl-k+ 11)
(defparameter +key-ctrl-l+ 12)
(defparameter +key-ctrl-n+ 14)
(defparameter +key-ctrl-p+ 16)
(defparameter +key-ctrl-q+ 17)
(defparameter +key-ctrl-t+ 20)
(defparameter +key-ctrl-u+ 21)
(defparameter +key-ctrl-w+ 23)
(defparameter +key-ctrl-x+ 24)
(defparameter +key-tab+ 9)
(defparameter +key-escape+ 27)

;;; ============================================================
;;; Key Binding Classes
;;; ============================================================

(defclass key-binding ()
  ((key :initarg :key :reader binding-key)
   (description :initarg :description :initform "" :reader binding-description))
  (:documentation "Base class for key bindings."))

(defclass simple-key-binding (key-binding)
  ((command-class :initarg :command-class :reader binding-command-class)
   (command-args :initarg :command-args :initform nil :reader binding-command-args))
  (:documentation "Simple binding: one key -> one command."))

(defclass contextual-key-binding (key-binding)
  ((context :initarg :context :reader binding-context)
   (command-class :initarg :command-class :reader binding-command-class)
   (command-args :initarg :command-args :initform nil :reader binding-command-args))
  (:documentation "Context-sensitive binding: key + context -> command."))

(defclass digit-key-binding (key-binding)
  ()
  (:documentation "Special binding for digit keys 1-9 -> buffer jump."))

(defclass enter-key-binding (key-binding)
  ()
  (:documentation "Special binding for Enter key (context-dependent)."))

(defclass navigation-key-binding (key-binding)
  ((direction :initarg :direction :reader binding-direction))
  (:documentation "Navigation binding that varies by focused pane."))

;;; ============================================================
;;; Generic Functions for Key Binding Protocol
;;; ============================================================

(defgeneric binding-matches-p (binding key context)
  (:documentation "Return T if BINDING matches KEY in CONTEXT."))

(defgeneric binding-commands (binding context)
  (:documentation "Return list of command instances for BINDING in CONTEXT."))

;;; ============================================================
;;; Method Implementations
;;; ============================================================

(defmethod binding-matches-p ((b simple-key-binding) key context)
  (declare (ignore context))
  (eql (binding-key b) key))

(defmethod binding-commands ((b simple-key-binding) context)
  (declare (ignore context))
  (list (apply #'make-instance
               (binding-command-class b)
               (binding-command-args b))))

(defmethod binding-matches-p ((b contextual-key-binding) key context)
  (and (eql (binding-key b) key)
       (eql (binding-context b) (getf context :focus))))

(defmethod binding-commands ((b contextual-key-binding) context)
  (declare (ignore context))
  (list (apply #'make-instance
               (binding-command-class b)
               (binding-command-args b))))

(defmethod binding-matches-p ((b digit-key-binding) key context)
  (declare (ignore context))
  ;; Digit keys are NOT handled by keymap - they go to input widget
  ;; Use Alt+digit or Ctrl+digit for buffer jumping instead
  nil)

(defmethod binding-commands ((b digit-key-binding) context)
  (declare (ignore context))
  ;; Disabled - digits go to input
  nil)

(defmethod binding-matches-p ((b enter-key-binding) key context)
  (declare (ignore context))
  (or (eql key #\Newline) 
      (eql key #\Return)
      (eql key 10)      ; LF
      (eql key 13)))

(defmethod binding-commands ((b enter-key-binding) context)
  (let ((focus (getf context :focus)))
    (when (eql focus :roster)
      (list (make-instance 'roster-open-selection)))))

(defmethod binding-matches-p ((b navigation-key-binding) key context)
  (declare (ignore context))
  (eql (binding-key b) key))

(defmethod binding-commands ((b navigation-key-binding) context)
  (declare (ignore context))
  (let ((dir (binding-direction b)))
    ;; Always move roster selection AND open the buffer
    (list (make-instance 'roster-move :dir dir)
          (make-instance 'roster-open-selection))))

;;; ============================================================
;;; Global Keymap
;;; ============================================================

(defvar *global-keymap* nil
  "List of key-binding objects for global key handling.")

(defun init-global-keymap ()
  "Initialize the global keymap with default bindings."
  (setf *global-keymap*
        (list
         ;; Quit
         (make-instance 'simple-key-binding
                        :key +key-ctrl-q+
                        :command-class 'quit-app
                        :description "Quit CLabber")
         ;; Split controls
         (make-instance 'simple-key-binding
                        :key +key-ctrl-w+
                        :command-class 'toggle-split
                        :description "Toggle split pane")
         (make-instance 'simple-key-binding
                        :key +key-ctrl-t+
                        :command-class 'toggle-orientation
                        :description "Toggle split orientation")
         ;; Focus cycling
         (make-instance 'simple-key-binding
                        :key +key-tab+
                        :command-class 'cycle-focus
                        :description "Cycle focus between panes")
         ;; Buffer close
         (make-instance 'simple-key-binding
                        :key +key-ctrl-k+
                        :command-class 'close-current-buffer
                        :description "Close current buffer")
         ;; Digit jump (special handling)
         (make-instance 'digit-key-binding
                        :key nil
                        :description "Jump to buffer 1-9")
         ;; Enter key (context-dependent)
         (make-instance 'enter-key-binding
                        :key #\Newline
                        :description "Open selection / Send message")
         ;; Swap panes
         (make-instance 'simple-key-binding
                        :key +key-ctrl-x+
                        :command-class 'swap-panes
                        :description "Swap split pane buffers")
         ;; Navigation - Ctrl+N/P moves roster and opens buffer
         (make-instance 'navigation-key-binding
                        :key +key-ctrl-n+
                        :direction 1
                        :description "Next roster item")
         (make-instance 'navigation-key-binding
                        :key +key-ctrl-p+
                        :direction -1
                        :description "Previous roster item"))))

;; Initialize on load
(init-global-keymap)

;;; ============================================================
;;; Key Lookup Function
;;; ============================================================

(defun key->commands (key st ly ui)
  "Convert KEY to list of commands based on current state.
   Uses CLOS method dispatch on key-binding objects."
  (declare (ignore st ui))
  (let ((context (list :key key
                       :focus (layout-focused-pane ly)
                       :split (layout-split-enabled-p ly))))
    (dolist (binding *global-keymap*)
      (when (binding-matches-p binding key context)
        (let ((cmds (binding-commands binding context)))
          (when cmds
            (return-from key->commands cmds)))))
    nil))
