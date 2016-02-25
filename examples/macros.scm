(import (rapid primitive))

(define-values (log)
  (case-lambda
   ((msg) (display msg) (newline))))

(define-syntax simple-template
  (syntax-rules ... ()
    ((simple-template a b) (string-append a b (string-append a a) a))))

(log (simple-template "1" "2"))
