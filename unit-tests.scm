;;; Unit testing

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

;;; Unit tests for simplification
(unit-tests simplified-sum
            (((0) => 0)
             ((0 0) => 0)
             ((0 1) => 1)
             ((1) => 1)
             ((1 x) => (sum 1 x))
             ((x y) => (sum x y))))

(unit-tests simplified-prod
           (((0) => 0)
            ((0 0) => 0)
            ((0 1) => 0)
            ((1) => 1)
            ((1 1) => 1)
            ((1 x) => x)
            ((2 x) => (prod 2 x))
            ((x y) => (prod x y))))

;++TODO
(unit-tests simplified-power
           ((0 0 => 1)
            (0 2 => 0)
            (1 2 => 1)
            (x 0 => 1)
            (x 1 => x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Random tests
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
  (sum (sum (var 'x)
            (const 5)
            (power (var 'x)
                   4))
       (prod  (var 'x)
              (const 5)
              (const 'x))))

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

(check (d (var 'x) (var 'x)) => 1)
(check (d (const 5) (var 'x)) => 0)



