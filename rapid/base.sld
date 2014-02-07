(define-library (rapid base)
  (export gensym atom? variable? if? lambda? case-lambda? set!? op define? *ops*)
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
    
    (define (case-lambda? expr)
      (tagged-list? expr 'case-lambda))
    
    (define (set!? expr)
      (tagged-list? expr 'set!))
    
    (define (op expr)
      (and (pair? expr)
        (cond
          ((assq (car expr) *ops*) => cdr)
          (else #f))))

    (define (define? expr)
      (tagged-list? expr 'define))

    (define *ops*
      (letrec-syntax ((define-operators
            (syntax-rules ()
              ((_) '())
              ((_ (operator name) definitions ...)
                `((operator . name) . ,(define-operators definitions ...)))
              ((_ operator definitions ...)
                `((operator . operator) . ,(define-operators definitions ...))))))
        (include "rapid/operator.scm")))))
