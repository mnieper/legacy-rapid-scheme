(import (rapid primitive))

(define-syntax f
  (syntax-rules ... ()
    ((f a b ...) (quote (f (a) ...)))))


(display (f 1 a b c))
(newline)
