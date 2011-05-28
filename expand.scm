;;; The expand function

;; Load list tools.
(load "list_tools.scm")

;; Expand polynomial poly
;; poly -> poly
(define (poly-expand poly)
  (cond
    ((const? poly) poly)
    ((var? poly) poly)
    ((derivative? poly) poly)
    ((sum? poly)
     (expanded-sum (map poly-expand (sum->args poly))))
    ((prod? poly)
     (expanded-prod (map poly-expand (prod->args poly))))
    ((power? poly)
     (expand-power (poly-expand (power-base poly)) (power-exponent poly)))))

;; Given a list of polynomials polys, construct an expanded sum of polys.
;; polys -> poly
(define (expanded-sum polys)
  (cond
    ((= (length polys) 1) (car polys)) ;This is simplification and not an expansion. Without it (sum (prod x y)) => (+ (* x y))
    (else (apply sum polys)))) 

;; Given a list of polynomials polys, construct an expanded product of polys.
;; polys -> poly
(define (expanded-prod polys)
  (cond
    ((= (length polys) 1) (car polys)) ;This is simplification and not an expansion. Without it (prod (sum x y)) => (* (+ x y))
    ((not (any sum? polys)) (apply prod polys))
    (else (sum-times-factor (find sum? polys)
                            (expanded-prod (remove-once (find sum? polys) polys))))))

;; Given a sum and a factor, construct their expanded product.
;; sum factor -> poly
(define (sum-times-factor the-sum factor)
  (cond
    ((poly-zero? factor) 0) ;This is simplification and not an expansion. Without it (prod 0 (sum x y))) => (+ (* 0 x) (* 0 y))
    ((poly-unity? factor) the-sum) ;This is simplification and not an expansion. Without it (prod 1 (sum x y))) => (+ (* 1 x) (* 1 y))
    ;++ TODO This can be shorter, Should be simpler
    (else (apply sum (let list-times-f ((l (sum->args the-sum))
                                        (f factor))
                       (append-map (lambda (x)
                                     (cond
                                       ((sum? f) (list-times-f (sum->args f) x))
                                       (else (list (expanded-prod f x)))))
                                   l))))))

(define (expand-power base exponent)
  (if (poly-zero? exponent) ;This is simplification and not an expansion. Without it (power poly 0) => (* )
      1
      (expanded-prod (make-list exponent base))))
