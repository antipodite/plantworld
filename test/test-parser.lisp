#|
Tests for L-system parser

Performs tests inside the package in which the code is defined, so it is
possible to test non-exported functions.
|#

(in-package #:parser)


;;;; Some L-systems for testing


(define-system *test1*
  "A(1)"
  "A(a) : a > 3 → B"
  "A(a) :       → BA(a + 1)")

(define-system *test2*
  "B(1)A(1,0)"
  "B(w) < A(x,y) : w<=1 → B(x)A(x+y,y+1)"
  "       B(x)   : x<1  → C"
  "       B(x)   :      → B(x-1)")

;; TODO in order for this to work #defines have to be supported
(define-system *fern*
  "A(0)"
  "A(d) : d > 0 → A(d-1)"
  "A(d) : d = 0 → F(0.5)[+A(d)]F(0.5)B(0)"
  "B(d) : d > 0 → B(d-1)"
  "B(d) : d = 0 → F(0.5)[-B(d)]F(0.5)A(0)"
  "F(a) :       → F(a*R)")


;;;; Test components of L-system parser


(def-suite parser-tests) ; TODO parent suite to run suite for each package tests
(in-suite parser-tests)


(test substitute-values
  (is (equal (substitute-values (string->infix "(x + y) * z")
                                '(($X 1) ($Y 5) ($Z 9)))
             '(|(| 1 + 5 |)| * 9)))
  (is (equal (substitute-values (string->infix "x")
                                '(($X 4)))
             '(4)))
  (is (equal (substitute-values (infix->prefix "(x + y) * z")
                                '(($X 1) ($Y 5) ($Z 9)))
             '(* (+ 1 5) 9))))

(test match-predecessor
  (fiveam:is-true
   (match-predecessor '("B(1)" "A(1,0)" nil)
                      '("B(w)" "A(x,y)" nil)))
  (fiveam:is-true
   (match-predecessor '(nil "B(1)" nil)
                      '(nil "B(a)" nil)))
  (fiveam:is-false
   (match-predecessor '(nil "B(1)" nil)
                      '(nil "C(a)" nil)))
  (fiveam:is-true
   (match-predecessor '("A" "B(2,3)" "C")
                      '(nil "B(x,y)" nil)))
  (fiveam:is-false
   (match-predecessor '(nil "B(1,2)" nil)
                      '("A" "B(x,y)" "C"))))

(test eval-expression
  (is (= 4 (eval-expression (infix->prefix "d + 2") '(($D 2))))))

(test bindings
  (is (equalp (get-predecessor-bindings '("B(1)" "A(1,0)" nil)
                                        '("B(w)" "A(x,y)" nil))
              '(($W 1) ($X 1) ($Y 0))))
  (fiveam:is-false (validate-bindings
                    (get-predecessor-bindings '(nil    "B(1)"   "A(1,0)")
                                              '("B(w)" "A(x,y)" nil))))
  (is (get-module-bindings "A(x + y)" "A(1, 2)")
      '(($X 1) ($Y 2))))

(test comma
  (is (equalp (|,| (+ 1 1) (> 2 4))
              '(2 |,| nil)))
  (is (equalp (|,| (* 2 (+ 1 1)) (/ (1- 3) 2))
              '(4 |,| 1)))
  (is (equalp (|,| 1 1)
              '(1 |,| 1))))
