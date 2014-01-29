(define-library (rapid base)
  (export gensym atom? variable? if? lambda? set!? op define? tagged-list? *ops*)
  (import (scheme base) (scheme case-lambda))
  (begin

    (define gensym-counter 0)
    
    (define gensym
      (case-lambda
        (() (gensym "g"))
        ((prefix)
          (set! gensym-counter (+ gensym-counter 1))
          (string->symbol (string-append prefix (number->string gensym-counter))))))
        
    (define (atom? expr)
      (or (variable? expr) (number? expr)))

    (define (tagged-list? expr tag)
      (and (pair? expr) (eq? (car expr) tag)))
      
    (define (variable? expr)
      (symbol? expr))
      
    (define (if? expr)
      (tagged-list? expr 'if))
      
    (define (lambda? expr)
      (tagged-list? expr 'lambda))
    
    (define (set!? expr)
      (tagged-list? expr 'set!))
    
    (define (op expr)
      (and (pair? expr)
        (cond
          ((assq (car expr) *ops*) => cadr)
          (else #f))))

    (define (define? expr)
      (tagged-list? expr 'define))

    (define *ops*
      (list
        '(= equality)
        '(+ sum)
        '(- difference)
        '(display display)
        '(exit exit)))))
