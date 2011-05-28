;;; The expand function

;; Load list tools.
(load "list_tools.scm")

;; Removing the first depth of a sum or product (or other thing)
; poly -> poly
;This macro is expensive, but it is so cool that I left it like this.
(define-syntax remove-depth-one
  (syntax-rules ()
    ((_ constructor recognizer selector poly) (apply constructor (append-map (lambda (x)
                                                                               (if (recognizer x)
                                                                                   (selector x)
                                                                                   (list x)))
                                                                             (selector poly))))
    ((_ poly) (remove-depth-one (get-constructor poly)
                                (get-recognizer poly)
                                get-args
                                poly))))
;A substitute for the macro
;(define (remove-depth-one-sum poly)
;  (apply sum (append-map (lambda (x)
;                           (if (sum? x)
;                               (sum->args x)
;                               (list x)))
;                         (sum->args poly))))
;(define (remove-depth-one-prod poly)
;  (apply prod (append-map (lambda (x)
;                            (if (prod? x)
;                                (prod->args x)
;                                (list x)))
;                          (prod->args poly))))

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
    (else (remove-depth-one (apply sum polys))))) 

;; Given a list of polynomials polys, construct an expanded product of polys.
;; polys -> poly
(define (expanded-prod polys)
  (cond
    ((= (length polys) 1) (car polys)) ;This is simplification and not an expansion. Without it (prod (sum x y)) => (* (+ x y))
    ((not (any sum? polys)) (remove-depth-one (apply prod polys)))
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
