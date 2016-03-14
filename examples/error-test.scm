(import (rapid))

(define (f x)
  (error "An error!"))

(define (g x)
  (+ 1 (f x)))

(g 10)
