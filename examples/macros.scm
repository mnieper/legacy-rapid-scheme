(import (rapid primitive))

(define-syntax h
  (syntax-rules
      ... ()
      ((h a b) a)))

(display (h "hi!" 2))
(newline)

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
