(in-package :plantworld)

(defconstant +left-context+ "<")
(defconstant +right-context+ ">")
(defconstant +colon+ )

;;; Tokeniser
(defparameter *test-def*
   "B(1)A(1,0)
    B(w) > A(x , y) : w <= 1 → B(x)A(x + y, y + 1)
    B(x)            : x < 1  → C
    B(x)                     → B(x - 1)")

#| Translated into sexprs:
(B 1)(A 1 0)
(B w) > (A x y) (if (<= w 1)) → (B x)(A (+ x y) (+ y 1))
(B x)           (if (< x 1))  → C
(B x)                         → (B (- x 1))
|#

