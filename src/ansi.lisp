(in-package #:clabber.ansi)

;;; ANSI Escape Sequence Library - CLOS-based
;;; Direct terminal control without ncurses dependency

(defparameter *escape* (code-char 27))

;;; Color class hierarchy

(defclass color ()
  ()
  (:documentation "Base class for terminal colors"))

(defclass indexed-color (color)
  ((index :initarg :index :accessor color-index :type (integer 0 255)))
  (:documentation "256-color palette color"))

(defclass rgb-color (color)
  ((red :initarg :red :accessor color-red :type (integer 0 255))
   (green :initarg :green :accessor color-green :type (integer 0 255))
   (blue :initarg :blue :accessor color-blue :type (integer 0 255)))
  (:documentation "24-bit true color"))

(defclass named-color (indexed-color)
  ((name :initarg :name :accessor color-name :type keyword))
  (:documentation "Named color with semantic meaning"))

;;; Color constructors

(defun make-indexed-color (index)
  (make-instance 'indexed-color :index index))

(defun make-rgb-color (r g b)
  (make-instance 'rgb-color :red r :green g :blue b))

(defun make-named-color (name index)
  (make-instance 'named-color :name name :index index))

;;; Standard color palette

(defparameter *color-palette* (make-hash-table :test 'eq))

(defun register-color (name index)
  (setf (gethash name *color-palette*) (make-named-color name index)))

(defun lookup-color (name-or-index)
  "Get a color object from name or index"
  (etypecase name-or-index
    (color name-or-index)
    (keyword (or (gethash name-or-index *color-palette*)
                 (make-indexed-color 7)))
    (integer (make-indexed-color name-or-index))))

;; Register standard colors
(register-color :black 0)
(register-color :red 1)
(register-color :green 2)
(register-color :yellow 3)
(register-color :blue 4)
(register-color :magenta 5)
(register-color :cyan 6)
(register-color :white 7)
(register-color :bright-black 8)
(register-color :bright-red 9)
(register-color :bright-green 10)
(register-color :bright-yellow 11)
(register-color :bright-blue 12)
(register-color :bright-magenta 13)
(register-color :bright-cyan 14)
(register-color :bright-white 15)
;; XMPP-specific semantic colors
(register-color :online 34)
(register-color :away 136)
(register-color :dnd 160)
(register-color :offline 245)
(register-color :encrypted 34)
(register-color :unencrypted 160)

;;; Generic functions for color output

(defgeneric emit-fg (color stream)
  (:documentation "Emit ANSI sequence for foreground color"))

(defgeneric emit-bg (color stream)
  (:documentation "Emit ANSI sequence for background color"))

(defmethod emit-fg ((color indexed-color) stream)
  (format stream "~C[38;5;~Dm" *escape* (color-index color)))

(defmethod emit-fg ((color rgb-color) stream)
  (format stream "~C[38;2;~D;~D;~Dm" *escape* 
          (color-red color) (color-green color) (color-blue color)))

(defmethod emit-bg ((color indexed-color) stream)
  (format stream "~C[48;5;~Dm" *escape* (color-index color)))

(defmethod emit-bg ((color rgb-color) stream)
  (format stream "~C[48;2;~D;~D;~Dm" *escape*
          (color-red color) (color-green color) (color-blue color)))

;;; Style class

(defclass text-style ()
  ((foreground :initarg :fg :accessor style-fg :initform nil)
   (background :initarg :bg :accessor style-bg :initform nil)
   (bold-p :initarg :bold :accessor style-bold-p :initform nil)
   (dim-p :initarg :dim :accessor style-dim-p :initform nil)
   (italic-p :initarg :italic :accessor style-italic-p :initform nil)
   (underline-p :initarg :underline :accessor style-underline-p :initform nil)
   (inverse-p :initarg :inverse :accessor style-inverse-p :initform nil))
  (:documentation "Text styling attributes"))

(defun make-style (&key fg bg bold dim italic underline inverse)
  (make-instance 'text-style
                 :fg (when fg (lookup-color fg))
                 :bg (when bg (lookup-color bg))
                 :bold bold :dim dim :italic italic
                 :underline underline :inverse inverse))

(defgeneric emit-style (style stream)
  (:documentation "Emit ANSI sequences for a style"))

(defmethod emit-style ((style text-style) stream)
  (when (style-bold-p style) (format stream "~C[1m" *escape*))
  (when (style-dim-p style) (format stream "~C[2m" *escape*))
  (when (style-italic-p style) (format stream "~C[3m" *escape*))
  (when (style-underline-p style) (format stream "~C[4m" *escape*))
  (when (style-inverse-p style) (format stream "~C[7m" *escape*))
  (when (style-fg style) (emit-fg (style-fg style) stream))
  (when (style-bg style) (emit-bg (style-bg style) stream)))

;;; Terminal class - encapsulates terminal state and operations

(defclass terminal ()
  ((stream :initarg :stream :accessor terminal-stream :initform *standard-output*)
   (width :initarg :width :accessor terminal-width :initform 80)
   (height :initarg :height :accessor terminal-height :initform 24))
  (:documentation "Terminal abstraction for output operations"))

(defgeneric term-cursor-to (terminal row col)
  (:documentation "Move cursor to position"))

(defgeneric term-cursor-home (terminal)
  (:documentation "Move cursor to top-left"))

(defgeneric term-cursor-hide (terminal)
  (:documentation "Hide cursor"))

(defgeneric term-cursor-show (terminal)
  (:documentation "Show cursor"))

(defgeneric term-clear-screen (terminal)
  (:documentation "Clear entire screen"))

(defgeneric term-clear-line (terminal)
  (:documentation "Clear current line"))

(defgeneric term-clear-to-end (terminal)
  (:documentation "Clear from cursor to end of line"))

(defgeneric term-reset (terminal)
  (:documentation "Reset all attributes"))

(defgeneric term-write (terminal text &key style)
  (:documentation "Write text with optional style"))

(defmethod term-cursor-to ((term terminal) row col)
  (format (terminal-stream term) "~C[~D;~DH" *escape* row col))

(defmethod term-cursor-home ((term terminal))
  (format (terminal-stream term) "~C[H" *escape*))

(defmethod term-cursor-hide ((term terminal))
  (format (terminal-stream term) "~C[?25l" *escape*))

(defmethod term-cursor-show ((term terminal))
  (format (terminal-stream term) "~C[?25h" *escape*))

(defmethod term-clear-screen ((term terminal))
  (format (terminal-stream term) "~C[2J" *escape*))

(defmethod term-clear-line ((term terminal))
  (format (terminal-stream term) "~C[2K" *escape*))

(defmethod term-clear-to-end ((term terminal))
  (format (terminal-stream term) "~C[K" *escape*))

(defmethod term-reset ((term terminal))
  (format (terminal-stream term) "~C[0m" *escape*))

(defmethod term-write ((term terminal) text &key style)
  (let ((stream (terminal-stream term)))
    (when style
      (emit-style style stream))
    (princ text stream)
    (when style
      (term-reset term))))

;;; Convenience functions - write directly to *terminal-io* for immediate output

(defparameter *terminal* nil "Current terminal instance")

(defun ensure-terminal ()
  (or *terminal* (setf *terminal* (make-instance 'terminal))))

(defun cursor-to (row col)
  (format *terminal-io* "~C[~D;~DH" *escape* (max 1 row) (max 1 col)))

(defun cursor-home ()
  (format *terminal-io* "~C[H" *escape*))

(defun cursor-hide ()
  (format *terminal-io* "~C[?25l" *escape*))

(defun cursor-show ()
  (format *terminal-io* "~C[?25h" *escape*))

(defun clear-screen ()
  (format *terminal-io* "~C[2J" *escape*))

(defun begin-sync-update ()
  "Begin synchronized update mode - terminal buffers output until end-sync-update"
  (format *terminal-io* "~C[?2026h" *escape*))

(defun end-sync-update ()
  "End synchronized update mode - terminal displays buffered content"
  (format *terminal-io* "~C[?2026l" *escape*))

(defun clear-line ()
  (format *terminal-io* "~C[2K" *escape*))

(defun clear-to-end ()
  (format *terminal-io* "~C[K" *escape*))

(defun reset ()
  (format *terminal-io* "~C[0m" *escape*))

(defun cursor-up (&optional (n 1))
  (format *terminal-io* "~C[~DA" *escape* n))

(defun cursor-down (&optional (n 1))
  (format *terminal-io* "~C[~DB" *escape* n))

(defun cursor-forward (&optional (n 1))
  (format *terminal-io* "~C[~DC" *escape* n))

(defun cursor-back (&optional (n 1))
  (format *terminal-io* "~C[~DD" *escape* n))

;;; Color convenience functions

(defun fg (color)
  (emit-fg (lookup-color color) *terminal-io*))

(defun bg (color)
  (emit-bg (lookup-color color) *terminal-io*))

(defun fg-rgb (r g b)
  (emit-fg (make-rgb-color r g b) *terminal-io*))

(defun bg-rgb (r g b)
  (emit-bg (make-rgb-color r g b) *terminal-io*))

(defun bold () (format *terminal-io* "~C[1m" *escape*))
(defun dim () (format *terminal-io* "~C[2m" *escape*))
(defun italic () (format *terminal-io* "~C[3m" *escape*))
(defun underline () (format *terminal-io* "~C[4m" *escape*))
(defun inverse () (format *terminal-io* "~C[7m" *escape*))

(defun color-code (name)
  "Get color code from name or return number as-is"
  (etypecase name
    (integer name)
    (keyword (let ((c (gethash name *color-palette*)))
               (if c (color-index c) 7)))))

;;; Convenience macro for styled output

(defmacro with-style ((&key fg bg bold dim italic underline inverse) &body body)
  "Execute body with specified text styling, then reset"
  `(progn
     ,@(when bold '((bold)))
     ,@(when dim '((dim)))
     ,@(when italic '((italic)))
     ,@(when underline '((underline)))
     ,@(when inverse '((inverse)))
     ,@(when fg `((fg ,fg)))
     ,@(when bg `((bg ,bg)))
     ,@body
     (reset)))
