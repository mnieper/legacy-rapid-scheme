(import (rapid primitive))

(define-values (i) (g 4))
  
(define-values (g)
  (case-lambda
   ((a) (+ a i))))



(g 1)
