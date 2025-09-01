#|
Parser for L-system definitions.
Infix expression parser modified from
https://people.eecs.berkeley.edu/~russell/code/logic/algorithms/infix.lisp

July 2025
|#

(in-package #:parser)

;;;;
;;;; Helper functions
;;;;


(defun flatten* (l)
  "Like alexandria:flatten but don't prune 'nil from result"
  (if (atom l)
      (list l)
      (append (flatten* (first l)) (when (cdr l) (flatten* (rest l))))))


(defun chunk (n seq)
  (loop for i from 0
        until (< (length seq) (+ i n))
        collect (subseq seq i (+ i n))))


(defun bookend (val list)
  "Add val to start and end of list"
  (reverse (cons val (reverse (cons val list)))))


(defun safe-concat (strings)
  "Non-variadic string concatenation, so we don't inadvertently run up against the arg limit"
  (reduce #'str:concat strings))


(defun remove-spaces (string)
  "Self-explanatory"
  (str:replace-all " " "" string))


(defun empty (list)
  "Return T if list is all nil"
  (every #'null list))


(defun substitute-values (expr var-values)
  "Substitute variables with values in expr according to var-values.
No scoping, all variables are global within the expression."
  (labels ((lookup-var (object)
             (let ((value (second (assoc object var-values))))
               (if value
                   value
                   object)))
           (traverse (tree)
             (cond ((atom tree)
                    (lookup-var tree))
                   (:else
                    (cons (traverse (car tree))
                          (mapcar #'traverse (cdr tree)))))))
    (traverse expr)))


(defun eval-expression (expr var-values)
  "Substitute values and eval parsed expression"
  (eval (substitute-values expr var-values)))


(defun module? (string) (ppcre:scan-to-strings "[A-Z](\\([^)]*\\))?" string))


(defun parse-system-string (string)
  "Parse an L-system string into a list of module and constant substrings."
  (let* ((mod-indices
           (ppcre:all-matches "[A-Z](\\([^)]*\\))?" string))
         (substrings
           (loop for (start end) on mod-indices while end
                 for substring = (subseq string start end)
                 if (not (str:empty? substring)) collect substring)))
    (values substrings mod-indices)))


(defun parse-production-string (rule)
  "Divide rule into pattern, condition, and result"
  (mapcar #'str:trim (ppcre:split ":|→" rule)))


(defun parse-predecessor-string (pred-string)
  "Parse contextual rules from predecessor part of production string.
-> string -> (string? string string?)

For example: A(d) will return (nil \"A(d)\" nil); A(b) < C(d) > E(f) will return
(\"A(b)\" \"C(d)\" \"E(f)\"), and so on.
"
  (flet ((split-and-trim ()
           (mapcar #'str:trim (ppcre:split "<|>" pred-string))))
    (cond
      ((and (find #\< pred-string) (find #\> pred-string))
       (split-and-trim))
      ((find #\< pred-string)
       (append (split-and-trim) '(nil)))
      ((find #\> pred-string)
       (cons nil (split-and-trim)))
      (t
       (list nil (str:trim pred-string) nil)))))


(defun yield-module-context (state-string)
  "Break state string into 3-element lists of modules.
-> string -> (string? string string?)

Lists are in the format (left-neighbour module right-neighbour). Note that non-module
characters, such as [, ], +, etc., do not count for matching, but do count as context.
Therefore, for example: F(1/2)[+A(d)]F(1/2)B(0) should break down into modules:
F(1/2), A(d), F(1/2), B(0).
The context for each of these modules should be:
(NIL F(1/2) NIL) (NIL A(d) NIL) (NIL F(1/2) B(0)) (F(1/2) B(0) NIL).
"
  (let ((substrings (parse-system-string state-string)))
    (remove-if (lambda (l) (null (second l)))
               (chunk 3 (mapcar #'module?
                                (bookend nil substrings))))))


(defun context-module (context)
  "Return module from 3 element context.
-> (string? string string?) -> string"
  (second context))


(defun context-left (context)
  (first context))


(defun context-right (context)
  (third context))


(defun regexify-predecessor-module (pred-module)
  "module? -> regex?"
  (ppcre:regex-replace-all "[a-z]" (ppcre:quote-meta-chars pred-module) "\\d+"))


(defun regexify-state-module (state-module)
  "module? -> regex?"
  (ppcre:regex-replace-all "\\d+" (ppcre:quote-meta-chars state-module) "[a-z]"))


(defun match-predecessor (mod-context pred-context)
  "Check whether predecessor rule matches module, including left and right neighbours.
-> (string? string string?) , (string? string string?) -> boolean

e.g. (B(1) A(1,0) NIL), (B(w) A(x,y) NIL) should return T. See tests/test-parser.lisp
for expected behaviour in all situations.
"
  (let ((mod-patterns (mapcar #'regexify-state-module mod-context)))
    (every #'identity
           (mapcar (lambda (m p)
                     (cond ((and (null p) (null m)) t)
                           ((and (stringp p) (stringp m)) (ppcre:scan-to-strings m p))
                           ((and (null p) (stringp m) t))
                           (:else nil)))
                   mod-patterns pred-context))))


;; TODO these 3 functions need to be modified so if there is an extra var
;; TODO i.e. a #define in Hannan 92, this is also returned with NIL as its
;; TODO value. Then I can add lookup for NIL vars in the L-system object

(defun get-module-bindings (state-mod pred-mod)
  "Return predecessor variable bindings as a list of ($var value) pairs.
-> string , string -> ((symbol symbol) ...)
The dollar sign is needed for interop with the infix expression parser.
e.g. B(3,5) , B(x,y) -> (($x 3) ($y 5))
"
   (remove nil (mapcar (lambda (a b) (when (not (equal a b))
                                  (list a b)))
                       (string->infix pred-mod)
                       (string->infix state-mod))))


(defun validate-binding (pair)
  "Check that single binding alist pair is in the right format.
(str num) -> (str num) or nil
"
  (destructuring-bind (var val) pair
    (and (ppcre:scan-to-strings "^\\$[A-Z]" (format nil "~a" var))
         (numberp val))))


(defun validate-bindings (var-values)
  "Verify that variable binding alist is in the correct format.
-> ((sym sym) ...) -> ((sym sym) ...) or nil
"
  (every #'identity (mapcar #'validate-binding var-values)))


(defun get-predecessor-bindings (mod-context pred-context)
  "As get-module-bindings, but for the entire state and predecessor context.
e.g. (B(w) A(x,y) NIL) , (B(1) A(1,0) NIL) -> '(($W 1) ($X 1) ($Y 0)).
"
  (mapcan #'get-module-bindings mod-context pred-context))


(defun parse-module (module)
  "Return module name and tokenised expression associated with module, if any.
-> string -> ((sym sym) ...) or nil
e.g. \"A(x + (y - z))\" -> '(+ x (- y z))
"
  (let ((paren-indices (ppcre:all-matches "\\(|\\)" module)))
    (if paren-indices
        (list (subseq module 0 (first paren-indices))
              (infix->prefix (subseq module ; Remove parens
                                     (1+ (first paren-indices))
                                     (1- (length module)))))
        (list module nil))))


(defun parse-successor-string (suc-string var-values)
  "Parse string into a list of modules, module expressions, and constants.
-> string -> string
Module expressions are further parsed from their infix forms into Lisp prefix
expressions ready for variable substitution and evaluation. Then they are
evaluated and the list of elements is concatenated back into a string and
returned.
e.g. B(x)A(x + y, y + 1) -> (\"B\" $X \"A\" (list (+ $X $Y) (+ $Y 1)));
     F(1/2)[+A(d)]F(1/2)B(0) ->  \"F(1/2)[+A(0)]F(1/2)B(0)\" with d = 0
"
  (labels ((eval-expr (expr vals) ; Ensure return values are bracketed
             (let ((result (eval (substitute-values expr vals))))
               (cond ((null result)
                      result) ; Because (atom nil) and (listp nil) are both true
                     ((atom result)
                      (list result))
                     (:else result)))))
    (let* ((substrings (parse-system-string suc-string))
           (elements (mapcan (lambda (string)
                               (if (module? string)
                                   (destructuring-bind (mod-name raw-expr) (parse-module string)
                                     (list mod-name (eval-expr raw-expr var-values)))
                                   (list string)))
                             substrings)))
      (remove-spaces
       (with-output-to-string (result)
         (loop for el in elements do (when el (format result "~a" el))))))))


;;;;
;;;; L-system object interface
;;;;


(defmacro define-system (name axiom &rest rules)
  "Conveniently define a system and expose it as a special variable.
For example:
(define-system *test1*
  \"A(1)\"
  \"A(a) : a > 3 → B\"
  \"A(a) :       → BA(a + 1)\")
"
  `(defparameter ,name
     (make-instance 'l-system
                    :axiom ,axiom
                    :rules (loop for rule in ',rules
                                 collect (make-instance 'production :string rule)))))


(defclass l-system ()
  ((axiom :initarg :axiom :accessor system-axiom :type list)
   (productions :initarg :rules :accessor system-productions :type list)
   (state :accessor system-state :type string)
   (iters :accessor system-iters :initform 0 :type integer))
  (:documentation "Base data structure storing L-system definition and state."))


(defmethod initialize-instance :after ((s l-system) &key)
  (setf (system-state s) (system-axiom s)))


(defmethod print-object ((s l-system) stream)
  (print-unreadable-object (s stream :type t)
    (format stream
            "~%Axiom: ~a~%Productions:~%~a~%State: ~a~%Iterations: ~a~%"
            (system-axiom s)
            (str:join #\Newline (mapcar #'production-string (system-productions s)))
            (system-state s)
            (system-iters s))))


(defmethod reset-system ((s l-system))
  (setf (system-state s) (system-axiom s)))


(defmethod apply-productions ((s l-system) state)
  (safe-concat
   (loop for state-context in (yield-module-context state)
         collect (let ((result
                         (loop for prod in (system-productions s)
                               collect (let* ((pred-context (production-predecessor prod))
                                              (var-values (get-predecessor-bindings state-context
                                                                                    pred-context))
                                              (match? (match-predecessor state-context pred-context)))
                                         (when (and match? (test-condition prod var-values))
                                           (return (parse-successor-string (production-successor prod)
                                                                           var-values)))))))
                   (if (empty result)
                       (context-module state-context)
                       result)))))


(defmethod apply-productions-debug ((s l-system) state)
  (loop for state-context in (yield-module-context state)
        do (progn (format t "~a~%" (str:repeat 90 "="))
                  (loop for prod in (system-productions s) do
                    (let* ((pred-context (production-predecessor prod))
                           (var-values (get-predecessor-bindings state-context pred-context))
                           (match? (match-predecessor state-context pred-context)))
                      (format t "~20A ~20A ~5A ~30A ~5A~40A~%"
                              state-context
                              pred-context
                              match?
                              (when match? var-values)
                              (when match? (test-condition prod var-values))
                              (when match? (parse-successor-string (production-successor prod)
                                                                   var-values)))))))
  :documentation "Output return values of the process as a table to the REPL for debugging")


;;; Production object


(defclass production ()
  ((string :reader production-string :initarg :string)
   (predecessor :accessor production-predecessor)
   (condition :accessor production-condition)
   (successor :accessor production-successor)))


(defmethod initialize-instance :after ((p production) &key)
  (destructuring-bind (predecessor condition successor)
      (parse-production-string (production-string p))
    (setf (production-predecessor p)
          (parse-predecessor-string predecessor))
    (setf (production-condition p)
          (infix->prefix condition))
    (setf (production-successor p)
          successor)))


(defmethod print-object ((p production) stream)
  (print-unreadable-object (p stream :type t)
    (format stream
            "~%Predecessor ~a~%Condition: ~a~%Successor: ~a~%"
            (production-predecessor p)
            (production-condition p)
            (production-successor p))))


(defmethod apply-production ((p production) state-context)
  (let* ((pred-context (production-predecessor p))
         (var-values (get-predecessor-bindings state-context
                                               pred-context)))
    (when (and (test-predecessor p state-context)
               (test-condition p var-values))
      (parse-successor-string (production-successor p) var-values))))


(defmethod test-predecessor ((p production) context)
  (match-predecessor context (production-predecessor p)))


(defmethod test-condition ((p production) var-values)
  (let ((condition (production-condition p)))
    (if (null condition)
        t
        (eval (substitute-values condition var-values)))))

;;;;
;;;; Parser for infix expressions
;;;;


(defun |,| (&rest exprs)
  "Handle comma-separated lists in parametric L-systems."
  (butlast (mapcan (lambda (expr) (list (eval expr) '|,|))
                   exprs)))


;; Supported operations
(defparameter *infix-ops*
  '((([ list match ]) ({ elts match }) (|(| nil match |)|))
    ((*) (/))
    ((+) (-))
    ((<) (>) (<=) (>=) (=) (/=))
    ((not not unary) (~ not unary))
    ((and) (& and) (^ and))
    ((or) (\|))
    ((=>))
    ((<=>))
    ((|,|))))


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
        (return
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


;;;;
;;;; Tokeniser for infix expressions
;;;;


(defun tokenise (string)
  (string->infix string))


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
            ((find ch "0123456789.e") (parse-any-number string i))
            ;;((find ch "0123456789") (parse-integer string :start i :junk-allowed t))
            ((symbol-char? ch) (parse-span string #'symbol-char? i))
            ((operator-char? ch) (parse-span string #'operator-char? i))
            (t (error "unexpected character: ~C" ch)))))


(defun parse-any-number (string i)
  "Expand number parsing to support floats, scientific, etc. - Isaac"
  (let ((j (position-if-not (lambda (c) (find c "1234567890.e"))
                            string :start i)))
    (values (org.mapcar.parse-number:parse-number (subseq string i j)) j)))


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
  (find ch '(#\Newline #\Tab #\Space)))


(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))
