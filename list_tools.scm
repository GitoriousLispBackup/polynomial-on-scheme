;;; Toolbox for lists

;; Load srfi 1
; for drracket
(require (lib "1.ss" "srfi"))
; for chicken
;(require-extension srfi-1)

;; Remove the first ocurence of an element in a list
;; x xs -> xs
(define (remove-once e l)
  (let remove-once-split ((p '()) (s l))
    (cond 
      ((null? s) p)
      ((eqv? e (car s)) (append p (cdr s)))
      (else (remove-once-split (append p (list (car s))) (cdr s))))))

;; It checks for set equality - but the sets
;; could contain repeated elements.
;; Very naive algorithm.
(define (set-equ? set1 set2)
  (if (null? set1)
    (null? set2)
    (letrec ((head1 (car set1))
             (tail1 (cdr set1))
             (rest-set2 (remove-once head1 set2)))
      (and (not (= (length set2) (length rest-set2)))
           (set-equ? tail1 rest-set2)))))

