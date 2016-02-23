(import (rapid primitive))

(define-syntax f
  (syntax-rules
      ... ()
      ((f) g)))

(f)

(display "ok")
(newline)
