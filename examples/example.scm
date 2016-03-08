(import (rapid primitive))

(define-values (g)
  (case-lambda
   ((a) (+ a a))))

(g 1)
