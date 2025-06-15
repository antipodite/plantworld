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

(defun count-distinct (list)
  "Return a tally of distinct items in list"
  (let ((unique (remove-duplicates list)))
    (loop for el in unique collect (list el (count el list)))))

(defun roulette-select (pairs)
  "Roulette wheel selection on (probability item) pairs"
  (let ((roll (random 1.0))
        (total 0))
    (loop for (prob item) in pairs
          do (incf total prob)
             (when (>= total roll)
               (return item)))))
