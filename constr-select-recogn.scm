;;; Costructors, Recognizers and Selectors
;;; for the basic polynomials

;; Constructors for polynomials

(define (const num)
  num)

(define (var sym)
  sym)

(define (sum . polys)
  (append '(+) polys))

(define (prod . polys)
  (append '(*) polys))

(define (power poly num)
  (list '** poly num))

;; Recognizers for polynomials

(define (const? poly)
  (number? poly))

(define (var? poly)
  (symbol? poly))

(define (sum? poly)
  (and (list? poly) (eqv? (first poly) '+)))

(define (prod? poly)
  (and (list? poly) (eqv? (first poly) '*)))

(define (power? poly)
  (and (list? poly) (eqv? (first poly) '**)))

;; Selectors for polynomials

(define (const->numeric const)
  const)

(define (var->symbol var)
  var)

(define (sum->args sum)
  (cdr sum))

(define (prod->args prod)
  (cdr prod))

(define (power-base pow)
  (second pow))

(define (power-exponent pow)
  (third pow))
  
;; Introspection on the basic types
;++ TODO some code is repeated here

(define (get-constructor poly)
  (cond
    ((const? poly) const)
    ((var? poly) var)
    ((sum? poly) sum)
    ((prod? poly) prod)
    ((power? poly) power)
    (else (error "get-constructor called on non basic type."))))
    
(define (get-recognizer poly)
  (cond
    ((const? poly) const?)
    ((var? poly) var?)
    ((sum? poly) sum?)
    ((prod? poly) prod?)
    ((power? poly) power?)
    (else (error "get-recognizer called on non basic type."))))

(define (get-args tree)
  (cdr tree))
