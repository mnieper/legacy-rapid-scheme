(import (rapid primitive))

;(define-values (x) (car (case-lambda (() y))))
;(define-values (f) (case-lambda (() (cons x y))))
;(define-values (y) (car (case-lambda (() x))))
;(define-values (t) (f))
;t

(define-values (y) (case-lambda (() 'z)))
(define-values (x) (set! x 4))

(set! x 10)
(set! x 11)

(set! x 12)
