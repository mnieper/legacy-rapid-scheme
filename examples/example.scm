(import (rapid primitive))

;(define-values (x) (car (case-lambda (() y))))
;(define-values (f) (case-lambda (() (cons x y))))
;(define-values (y) (car (case-lambda (() x))))
;(define-values (t) (f))
;t

(define-values (f) y #;(case-lambda ((x) y)))

(define-values (y) 10)

(f)
