;;; Grouping functionality

;; Load list tools.
(load "list_tools.scm")

;++ TODO
;; Groups iterated sums (eg sum x x) in a product (eg prod 2 x)
;;  It runs over the list once for each element and accumulates repeated
;; elements. It seems that the complexity is O(n^2). But it's small in
;; memory footprint. And it's the shortest algo that I could think of.
;;  It supposes that it's given a flat sum/prod (no nesting of the same
;; operators).
;;  It supposes that it's given nonempty list of polynomials.
(define (group-sum polys)
  (let scan ((scanned '())
             (current (car polys))
             (to-scan (cdr polys)))
    (if (null? to-scan)
      (append scanned (list current))
      (scan (append scanned (list current))
            (car to-scan)
            (cdr to-scan)))))

;++ TODO
;; Grouping for products (prod x x in pow x 2)
(define (group-prod polys)
  (let scan ((scanned '())
             (current (car polys))
             (to-scan (cdr polys)))
    (if (null? to-scan)
      (append scanned (list current))
      (scan (append scanned (list current))
            (car to-scan)
            (cdr to-scan)))))

;; Checks if two monoms are of the same signature.
(define (same-signature monom1 monom2)
  (set-equ? (signature monom1)
            (signature monom2)))

;; Gets the signature of a monom
;;  It supposes that the argument does not contain any sums (a monom).
;; It returns a list of variables repeated as many times as their power.
(define (signature monom)
  (cond
    ((const? monom) '())
    ((var? monom) (list monom))
    ((prod? monom) (append-map signature (prod->args monom)))
    ((power? monom) (concatenate (make-list (power-exponent monom)
                                            (signature (power-base monom)))))))

;; Gets the numerical factor of a monom
;;  It supposes that the argument does not contain any sums (a monom).
;; It depend on the internal representation of const.
(define (factor monom)
  (cond
    ((const? monom) (const->numeric monom))
    ((var? monom) 1)
    ((prod? monom) (apply * (map factor (prod->args monom))))
    ((power? monom) (apply * (make-list (power-exponent monom)
                                        (factor (power-base monom)))))))
