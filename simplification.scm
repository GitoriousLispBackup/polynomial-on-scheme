;;; Simplification function

;; Load list tools.
(load "list_tools.scm")

;; Simplify polynomial poly.
;; poly -> poly
(define (simplify poly)
  (cond
    ((const? poly) poly)
    ((var? poly) poly)
    ((sum? poly)
     (simplified-sum (map simplify (sum->args poly))))
    ((prod? poly)
     (simplified-prod (map simplify (prod->args poly))))
    ((power? poly)
     (simplified-power (simplify (power-base poly))
                       (power-exponent poly)))
    ((derivative? poly) poly)))

;; Given a list of polynomials polys, construct a simplified sum of polys.
;; polys -> poly
(define (simplified-sum polys)
  (let ((p (remove poly-zero? polys)))
    (cond
      ((null? p) 0)
      ((= (length p) 1) (car p))
      (else (apply sum p)))))

;; Given a list of polynomials polys, construct a simplified prod of polys.
;; polys -> poly
(define (simplified-prod polys)
  (if
   (any poly-zero? polys)
   (const 0)
   (let ((p (remove poly-unity? polys)))
     (cond
       ((null? p) 1)
       ((= (length p) 1) (car p))
       (else (apply prod p))))))

;; Given polynomial base and natural integer exponent, construct a simplified power with base base and exponent exponent.
;; poly, natural -> poly
(define (simplified-power base exponent)
  (cond
    ((= exponent 1) base)
    ((zero? exponent) (const 1))
    ((poly-zero? base) (const 0))
    ((poly-unity? base) (const 1))
    (else (power base exponent))))
