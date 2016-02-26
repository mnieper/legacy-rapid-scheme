(import (rapid primitive))

(define-values (log)
  (case-lambda
   ((msg) (display msg) (newline))))

(define-syntax simple-template
  (syntax-rules ... ()
		((simple-template a c ... b)
		 (quote (string-append (... (a ...)) b (string-append #(a a) . b) a)))))

(log (simple-template "1" 'u 'v "2"))
