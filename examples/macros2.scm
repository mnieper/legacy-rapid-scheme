(import (rapid primitive))

(define-syntax define-macro
  (syntax-rules ... (case-lambda)
    ((define-macro op ellipsis ()
       (case-lambda
	(%formals %expression)
	...))
     2)))

(define-macro $cons ... ()
  (case-lambda
   ((h t) '(h . t))))
