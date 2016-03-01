(import (rapid primitive)
	(rapid let))

;; Secret literals
(define-syntax :call (syntax-rules ... ()))
(define-syntax :prepare (syntax-rules ... ()))

(define-syntax ck
  (syntax-rules ... (quote)
    ((ck () 'v) v)
    ((ck (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))
    ((ck s "arg" (op va ...))
     (op :call s va ...))
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))
    ((ck s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))
    ((ck s (op ea ...))
     (op :prepare s ea ... ))))

(let ()
  (define-syntax m-expression
    (syntax-rules ... (quote :prepare :call)
      ((m-expression :prepare s expression)
       (ck s "arg" (m-expression) 'expression))
      ((m-expression :call s 'expression)
       (let ()
	 (m-list 'let '() (m-list 'define 'x expression) 'x)))
      ((m-expression expression)
       (ck () (m-expression expression)))))
  
  (m-expression '#t))
