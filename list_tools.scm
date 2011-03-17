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
