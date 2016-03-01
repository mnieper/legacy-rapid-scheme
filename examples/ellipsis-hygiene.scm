(import (rapid primitive))

(define-syntax foo
  (syntax-rules ... ()
    ((foo body)
     (define-syntax bar
       (syntax-rules ...1 ()
         ((bar) body))))))

(foo (a ...1))
