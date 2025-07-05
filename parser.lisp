(in-package :plantworld)

#|
Parser for L-system definitions.
Infix expression parser from
https://people.eecs.berkeley.edu/~russell/code/logic/algorithms/infix.lisp
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

(defparameter *test-expr1*
  "A(1)
   A(a) : a > 3 → B
   A(a) :       → BA(a + 1)")

(defparameter *test-expr2*
  "B(1)A(1,0)
   B(w) < A(x,y) : w <= 1 → B(x)A(x + y, y + 1)
   B(x)          : x <  1 → C
   B(x)                   → B(x - 1)")


;;;; Process L-system definitions

(defun get-axiom (defn) (first (str:lines defn)))
(defun get-rules (defn) (rest (str:lines defn)))

(defun split-rule (rule)
  "Divide rule into pattern, condition, and result"
  (mapcar #'str:trim (ppcre:split ":|→" rule)))

(defun trim-pattern (pattern)
  "Remove < and > from pattern segment of production.
Since we use pattern matching for positional rules, > and < are irrelevant."
  (ppcre:regex-replace-all "\\s+|<|>" pattern ""))

(defun pattern->regex (pattern)
  "Convert an L-system pattern to a regex for rule matching"
  (ppcre:regex-replace-all "[a-z]" (ppcre:quote-meta-chars pattern) "\\d+"))

(defun remove-matches (list-a list-b)
  "Step through each element of a and b, removing from a and b if identical."
    (loop for a in list-a
          for b in list-b
          when (not (equalp a b)) collect (list a b)))

(defun match-pattern (pattern string)
  (let ((trimmed (trim-pattern pattern)))
    (when (ppcre:scan (pattern->regex trimmed) string)
      (list (string->infix string) (string->infix trimmed)))))

;;;; Parser for infix expressions

(defun infix->prefix (infix)
  "Convert an infix expression to prefix."
  (when (stringp infix) (setf infix (string->infix infix)))
  ;; INFIX is a list of elements; each one is in prefix notation.
  ;; Keep reducing (most tightly bound first) until there is only one 
  ;; element left in the list.  Example: In two reductions we go:
  ;; (a + b * c) => (a + (* b c)) => ((+ a (* b c)))
  (loop 
    (when (not (length>1 infix)) (RETURN (first infix)))
    (setf infix (reduce-infix infix))))

(defun reduce-infix (infix)
  "Find the highest-precedence operator in INFIX and reduce accordingly."
  (dolist (ops *infix-ops* (error "Bad syntax for infix expression: ~S" infix))
    (let* ((pos (position-if #'(lambda (i) (assoc i ops)) infix
                             :from-end (eq (op-type (first ops)) 'MATCH)))
           (op (when pos (assoc (elt infix pos) ops))))
      (when pos
        (RETURN
         (case (op-type op)
           (MATCH (reduce-matching-op op pos infix))
           (UNARY (replace-subseq infix pos 2 
                                  (list (op-name op) 
                                        (elt infix (+ pos 1)))))
           (BINARY (replace-subseq infix (- pos 1) 3
                                   (list (op-name op)
                                         (elt infix (- pos 1)) 
                                         (elt infix (+ pos 1)))))))))))

(defun op-token (op) (first op))
(defun op-name (op) (or (second op) (first op)))
(defun op-type (op) (or (third op) 'BINARY))
(defun op-match (op) (fourth op))

(defun replace-subseq (sequence start length new)
  (nconc (subseq sequence 0 start) (list new)
         (subseq sequence (+ start length))))

(defun reduce-matching-op (op pos infix)
  "Find the matching op (paren or bracket) and reduce."
  ;; Note we don't worry about nested parens because we search :from-end
  (let* ((end (position (op-match op) infix :start pos))
         (len (+ 1 (- end pos)))
         (inside-parens (remove-commas (infix->prefix (subseq infix (+ pos 1) end)))))
    (cond ((not (eq (op-name op) '|(|)) ;; handle {a,b} or [a,b]
           (replace-subseq infix pos len 
                           (cons (op-name op) inside-parens))) ; {set}
          ((and (> pos 0)  ;; handle f(a,b)
                (function-symbol? (elt infix (- pos 1))))
           (handle-quantifiers
            (replace-subseq infix (- pos 1) (+ len 1)
                           (cons (elt infix (- pos 1)) inside-parens))))
          (t ;; handle (a + b)
           (assert (= 1 (length inside-parens)))
           (replace-subseq infix pos len (first inside-parens))))))
                    
(defun remove-commas (exp)
  "Convert (|,| a b) to (a b)."
  (cond ((eq (op exp) '|,|) (nconc (remove-commas (arg1 exp) )
                                   (remove-commas (arg2 exp))))
        (t (list exp))))

(defun handle-quantifiers (exp)
  "Change (FORALL x y P) to (FORALL (x y) P)."
  (if (member (op exp) '(FORALL EXISTS))
    `(,(op exp) ,(butlast (rest exp)) ,(last1 exp))
    exp))


(defun op (exp) "Operator of an expression" (if (listp exp) (first exp) exp))
(defun args (exp) "Arguments of an expression" (if (listp exp) (rest exp) nil))
(defun arg1 (exp) "First argument" (first (args exp)))
(defun arg2 (exp) "Second argument" (second (args exp)))

(defun length>1 (list)
  "Is this a list of 2 or more elements?"
  (and (consp list) (cdr list)))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun last1 (list)
  "Return the last element of a list."
  (first (last list)))

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
