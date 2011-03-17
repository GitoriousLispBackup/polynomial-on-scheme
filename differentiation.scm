;;; Differentiation

;; Construct a derivative.
;; poly var -> poly
(define (derivative poly x)
  (list 'd poly x))

;; Recognize a derivative.
;; poly -> bool
(define (derivative? poly)
  (and (list? poly) (eqv? (first poly) 'd)))

;; Calculate a derivative.
;; poly var -> poly
;; How: just applies the standard differentiation rules.
(define (d poly x)
  (cond
    ((const? poly)
     0)
    ((var? poly) 
     (if (eqv? poly x) 
         1 
         (derivative poly x)))
    ((sum? poly) 
     (sum (map (lambda (p)
                 (d p x))
               (cdr poly))))
    ((prod? poly) 
     (sum (map (lambda (p)
                 (prod (d p x) (remove-once p poly)))
               (cdr poly))))
    ((power? poly)
     (prod (power-exponent poly)
           (power (power-base poly) 
                  (- (power-exponent poly) 1))
           (d (power-base poly) x)))))


