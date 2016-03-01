(import (rapid primitive))

(define-syntax :call (syntax-rules ... ()))
(define-syntax :prepare (syntax-rules ... ()))

(define-syntax m-expression
  (syntax-rules ... (quote :prepare :call)
    ((m-expression :prepare s expression)
     (ck s "arg" (m-expression) 'expression))
    ((m-expression :call s 'expression)
     (let ()
       (m-list 'let '() (m-list 'define 'x expression) 'x)))
    ((m-expression expression)
     (ck () (m-expression expression)))))


#;(define-syntax m
  (syntax-rules ... ()
    ((m)
     (let ()
       (bla) '()))))

(m-expression :call s '#t)
