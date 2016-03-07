(import (rapid primitive))

(define-values (f)
  (case-lambda
   ((x) (+ x x))))

(display (f 1))
(newline)
