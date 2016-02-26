(import (rapid primitive))

(define-values (log)
  (case-lambda
   ((msg) (display msg) (newline))))

(define-syntax simple-template
  (syntax-rules ... ()
		((simple-template a c ... b) "ok")))
		 
(log (simple-template "1" (quote u) v "2"))
