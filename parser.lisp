(in-package :plantworld)

#|
Parser for L-system definitions.
Adapted from https://people.eecs.berkeley.edu/~russell/code/logic/algorithms/infix.lisp
|#

;;;; Supported operations
(defparameter *infix-ops*
  '(((*) (/))
    ((+) (-))
    ((<) (>) (<=) (>=) (=) (/=))
    ((not not unary) (~ not unary))
    ((and) (& and) (^ and))
    ((or) (\|))
    ((=>))
    ((<=>))
    ((|,|))))

(defparameter *test-expr*
  "A(1)
   A(a) : a > 3 → B
   A(a) :       → BA(a + 1)") 
  

;;;; Tokeniser for infix expressions

(defun string->infix (string &optional (start 0))
  "Convert a string to a list of tokens."
  (multiple-value-bind (token i) (parse-infix-token string start)
    (cond ((null token) nil)
          ((null i) (list token))
          (t (cons token (string->infix string i))))))

(defun parse-infix-token (string start)
  "Return the first token in string and the position after it, or nil."
    (let* ((i (position-if-not #'whitespace? string :start start))
           (ch (if i (char string i))))
      (cond ((null i) (values nil nil))
            ((find ch "+-~()[]{},") (values (intern (string ch)) (+ i 1)))
            ((find ch "0123456789") (parse-integer string :start i :junk-allowed t))
            ((symbol-char? ch) (parse-span string #'symbol-char? i))
            ((operator-char? ch) (parse-span string #'operator-char? i))
            (t (error "unexpected character: ~C" ch)))))

(defun parse-span (string predicate i)
  (let ((j (position-if-not predicate string :start i)))
    (values (make-logic-symbol (subseq string i j)) j)))

(defun make-logic-symbol (string)
  "Convert string to symbol, preserving case."
  ;; Check that this is be
  (cond ((find string '("and" "or" "not") :test #'string=)
         string)
        ((lower-case-p (char string 0))
         (concat-symbol "$" (string-upcase string)))
        (:else (intern (string-upcase string)))))

(defun operator-char? (s) (find s "<=>&^|*/,"))

(defun symbol-char? (s) (or (alphanumericp s) (find s "$!?%:→")))

(defun function-symbol? (s) (and (symbolp s) (not (member s '(and or not ||)))))

(defun whitespace? (ch)
  "Return T if char is space. tab or newline."
  (find ch " 	
"))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))
