(import (rapid primitive))

(define-syntax foo
  (syntax-rules ()
    ((foo (a . b))
     (display "hi!\n"))))

(foo (1 2 . 3))
(newline)
