(import (rapid primitive))

;(define-values (x) (car (case-lambda (() y))))
;(define-values (f) (case-lambda (() (cons x y))))
;(define-values (y) (car (case-lambda (() x))))
;(define-values (t) (f))
;t

(define-values (u v) (car))

(define-values (a) (u v))
(define-values (f)
  (case-lambda
   (args a)))

(f 1)
