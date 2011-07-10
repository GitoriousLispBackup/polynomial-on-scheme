;;; Unit testing

;++ TODO The test coverage is bad. Write more tests.

;; Load the polynomial stuff.
(load "polynom.scm")

;; Load the unit test framework.
(load "unit-test.scm")

;; Reporting only failed tests
(check-set-mode! 'report-failed)

;; Define variables that will be frequently used during tests.
(define x
  (var 'x))

(define y
  (var 'y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Systematical tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Unit tests for constructors
(check (sum x (sum x)) => (prod 2 x))
(check (sum x (sum x) (prod x)) => (prod 3 x))
(check (sum x (sum x) (prod 2 x)) => (prod 4 x))
(check (sum x (sum x) (prod x 2)) => (prod 4 x))
(check (prod x (prod x) (power x 2)) => (power x 4))

;;; Unit tests for simplification
(check (simplified-sum '(0)) => 0)
(check (simplified-sum '(0 0)) => 0)
(check (simplified-sum '(0 1)) => 1)
(check (simplified-sum '(1)) => 1)
(check (simplified-sum '(1 x)) => (sum 1 x))
(check (simplified-sum '(x y)) => (sum x y))

(check (simplified-prod '(0)) => 0)
(check (simplified-prod '(0 0)) => 0)
(check (simplified-prod '(0 1)) => 0)
(check (simplified-prod '(1)) => 1)
(check (simplified-prod '(1 1)) => 1)
(check (simplified-prod '(1 x)) => x)
(check (simplified-prod '(2 x)) => (prod 2 x))
(check (simplified-prod '(x y)) => (prod x y))

(check (simplified-power 0 0) => 1)
(check (simplified-power 0 2) => 0)
(check (simplified-power 1 2) => 1)
(check (simplified-power x 0) => 1)
(check (simplified-power x 1) => x)

;;; Unit tests for expand
(check (poly-expand (sum x y)) => (sum x y))
(check (poly-expand (prod x y)) => (prod x y))
(check (poly-expand (prod 1 (sum x y))) => (sum x y))
(check (poly-expand (prod 0 (sum x y))) => 0)
(check (poly-expand (sum (prod x))) => x)
(check (poly-expand (prod (sum x))) => x)

;;; Unit tests for differentiation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define c3
  (const 3))

(define <x+y>
  (sum x y))

(define 3*<x+y>
  (prod c3 <x+y>))

(define <3*x+3*y>
  (sum (prod c3 x)
       (prod c3 y)))

(define mypoly
  (sum (sum x
            (const 5)
            (power x
                   4))
       (prod  x
              (const 5)
              x)))

;;; Unit tests for simplification
(check (simplify mypoly) => (sum x 5 (power x 4) (prod x 5 x)))

(check (simplify (sum 0 0)) => 0)
(check (simplify (sum <x+y> 0)) => <x+y>)
(check (simplify (sum mypoly 0)) => (simplify mypoly))

(check (simplify (prod 0 0)) => 0)
(check (simplify (prod <x+y> 0)) => 0)
(check (simplify (prod mypoly 0)) => 0)

(check (simplify (prod 1 1)) => 1)
(check (simplify (prod <x+y> 1)) => <x+y>)
(check (simplify (prod mypoly 1)) => (simplify mypoly))

(check (simplify (power <x+y> 0)) => 1)
(check (simplify (power mypoly 0)) => 1)

(check (simplify (power <x+y> 1)) => <x+y>)
(check (simplify (power mypoly 1)) => (simplify mypoly))

;;; Unit tests for expand

;;; Unit tests for differentiation
(check (d x x) => 1)
(check (d c3 x) => 0)
