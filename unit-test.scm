;;; Unit test framework

;; Load srfi 78.
; for drracket
(require (lib "78.ss" "srfi"))  (load "check.scm")
; for chicken
;(require-extension srfi-78)

;; Load list tools.
(load "list_tools.scm")

;; Another way to write check
;; (check-proc-on-case proc (arg1 arg2 ... => result)) ==> (check (proc arg1 arg2 ...) => result)
(define-syntax check-proc-on-case
  (syntax-rules ()
    ((_ proc test-case)
     (let ((t-case (quote test-case)))
       (display t-case)
       (check (apply proc (reverse (cddr (reverse t-case))))
              =>
              (eval (last t-case)))))))
              
(define-syntax unit-tests
  (syntax-rules ()
    ((_ proc test-cases)
     (let ((t-cases (quote test-cases)))
       (let ((to-eval (map (lambda (c) (list 'check-proc-on-case proc c))
                            t-cases)))
         (eval `(begin ,@to-eval)))))))
