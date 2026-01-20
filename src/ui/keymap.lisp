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

;; Alt+digit keys (escape sequences: ESC followed by digit)
;; Note: read-key-with-escape returns (cons 27 char), not a list
(defparameter +key-alt-1+ (cons 27 #\1))
(defparameter +key-alt-2+ (cons 27 #\2))
(defparameter +key-alt-3+ (cons 27 #\3))
(defparameter +key-alt-4+ (cons 27 #\4))
(defparameter +key-alt-5+ (cons 27 #\5))
(defparameter +key-alt-6+ (cons 27 #\6))
(defparameter +key-alt-7+ (cons 27 #\7))
(defparameter +key-alt-8+ (cons 27 #\8))
(defparameter +key-alt-9+ (cons 27 #\9))

;; Alt+letter keys for participant navigation
(defparameter +key-alt-j+ (cons 27 #\j))  ; Participant down
(defparameter +key-alt-k+ (cons 27 #\k))  ; Participant up
(defparameter +key-alt-o+ (cons 27 #\o))  ; Open private chat with participant

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

(defclass alt-key-binding (key-binding)
  ((command-class :initarg :command-class :reader binding-command-class)
   (command-args :initarg :command-args :initform nil :reader binding-command-args))
  (:documentation "Alt+key binding (ESC followed by character)."))

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

(defun key-to-digit (ch)
  "Convert a key (character or integer) to digit 1-9, or NIL."
  (cond
    ((characterp ch) (let ((d (digit-char-p ch))) (when (and d (<= 1 d 9)) d)))
    ((integerp ch) (let ((d (- ch 48))) (when (<= 1 d 9) d)))  ; 48 = ASCII '0'
    (t nil)))

(defmethod binding-matches-p ((b digit-key-binding) key context)
  (declare (ignore context))
  ;; Match Alt+digit (stored as cons of ESC + digit char/int)
  (and (consp key)
       (eql (car key) 27)
       (key-to-digit (cdr key))))

(defmethod binding-commands ((b digit-key-binding) context)
  ;; Extract digit from Alt+digit key stored in context
  (let* ((key (getf context :key))
         (digit (when (consp key) (key-to-digit (cdr key)))))
    (when digit
      (list (make-instance 'jump-open-buffer :index digit)))))

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

(defun alt-key-equal (expected actual)
  "Compare Alt+key bindings, handling char vs int mismatch.
   EXPECTED is (27 . char), ACTUAL may be (27 . int)."
  (and (consp expected) (consp actual)
       (eql (car expected) (car actual))
       (let ((exp-ch (cdr expected))
             (act-ch (cdr actual)))
         (or (eql exp-ch act-ch)
             (and (characterp exp-ch) (integerp act-ch)
                  (eql (char-code exp-ch) act-ch))
             (and (integerp exp-ch) (characterp act-ch)
                  (eql exp-ch (char-code act-ch)))))))

(defmethod binding-matches-p ((b alt-key-binding) key context)
  (declare (ignore context))
  (alt-key-equal (binding-key b) key))

(defmethod binding-commands ((b alt-key-binding) context)
  (declare (ignore context))
  (list (apply #'make-instance 
               (binding-command-class b)
               (binding-command-args b))))

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
                        :description "Previous roster item")
         ;; Participant navigation - Alt+J/K/O
         (make-instance 'alt-key-binding
                        :key +key-alt-j+
                        :command-class 'participant-move
                        :command-args '(:dir :down)
                        :description "Next participant")
         (make-instance 'alt-key-binding
                        :key +key-alt-k+
                        :command-class 'participant-move
                        :command-args '(:dir :up)
                        :description "Previous participant")
         (make-instance 'alt-key-binding
                        :key +key-alt-o+
                        :command-class 'participant-open-private
                        :description "Open private chat with participant"))))

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
