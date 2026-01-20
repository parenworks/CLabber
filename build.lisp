;;; build.lisp - Build script for CLabber executable
;;; Run with: sbcl --non-interactive --load build.lisp

(require :asdf)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first:
  curl -O https://beta.quicklisp.org/quicklisp.lisp
  sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit")))

;; Add project to ASDF registry
(push *default-pathname-defaults* asdf:*central-registry*)

;; Load the system (ql:quickload will fetch missing dependencies)
(ql:quickload :clabber)

;; Build the executable
;; :save-runtime-options nil prevents SBCL from processing --help/--version
(sb-ext:save-lisp-and-die "clabber"
                          :toplevel #'clabber:main
                          :executable t
                          :compression t
                          :save-runtime-options nil)
