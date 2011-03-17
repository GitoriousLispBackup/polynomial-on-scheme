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

