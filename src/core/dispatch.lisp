;;;; dispatch.lisp - Command dispatch for CLabber

(in-package #:clabber)

(defun execute-commands (cmds st ly engine)
  "Execute a list of commands, threading state and layout through."
  (dolist (c cmds (values st ly))
    (multiple-value-setq (st ly) (execute c st ly engine))))
