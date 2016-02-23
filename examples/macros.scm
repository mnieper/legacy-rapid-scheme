(import (rapid primitive))

(define-values (g) "ok")

(define-syntax f
  (syntax-rules
      ... ()
      ((f) g)))

((case-lambda
  ((g)
   (display (f))))
 3)

(newline)
