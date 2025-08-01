;;;; Compilation script to produce a binary.
;;;; Run: sbcl --script compile.lisp

;; Load SBCL config so we have access to quicklisp
(load "~/.sbclrc")

;; Remove previous binary if it exists
(let ((binary-path (probe-file (merge-pathnames (truename *default-pathname-defaults*)
                                                "plantworld"))))
  (when binary-path
    (format t "Found previous binary ~A, removing...~%~%" binary-path)
    (sb-ext:run-program "/bin/rm" (list (namestring binary-path)))))

(ql:quickload '("plantworld" "plantworld/tests"))

(in-package :plantworld)

(sb-ext:save-lisp-and-die "plantworld"
                          :toplevel #'run
                          :executable t
                          :compression t)
