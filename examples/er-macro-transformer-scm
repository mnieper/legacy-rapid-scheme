(import (rapid primitive))

(define-values (bar)
  (case-lambda
    (()
     (display "hi!\n"))))

(define-syntax foo
  (er-macro-transformer
   (lambda (syntax rename compare)
     (derive-syntax 'bar syntax))))

((foo))
