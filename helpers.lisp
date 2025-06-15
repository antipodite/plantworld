;;;;
;;;; Helper functions
;;;;

(in-package :plantworld)

(defun chunk (n seq)
  (loop for i from 0
        until (< (length seq) (+ i n))
        collect (subseq seq i (+ i n))))

(defun as-list (string)
  "Convert a string to a list of single char strings"
  (mapcar (lambda (c) (string c)) (coerce string 'list)))

(defun as-string (list)
  "Convert a list of single char strings to a string"
  (apply #'concatenate 'string list))

(defun print-hash (hash &optional (stream t))
  (format stream "{~{~A~^ ~}}"
          (loop for key in (hash-keys hash)
                for val = (gethash key hash)
                collect (format nil "~A => ~A" key val))))
