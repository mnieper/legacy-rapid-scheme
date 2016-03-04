(import (rapid primitive))

(define-syntax x
  (syntax-rules ()
    ((x . _) _)))

(x display "hi!\n")
