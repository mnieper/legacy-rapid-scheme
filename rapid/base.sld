(define-library (rapid base)
  (export uncons make-gensym atom? variable? case-lambda? if? lambda? set!? op define? tagged-list? *ops* tagged-pair?)
  (import (scheme base) (scheme case-lambda))
  (begin

    (define (make-gensym)
      (define counter 0)
      (letrec ((gensym
          (case-lambda
            (() (gensym "g"))
            ((prefix)
              (set! counter (+ counter 1))
              (string->symbol (string-append prefix (number->string counter)))))))
        gensym))
    
    (define (uncons expr)
      ;
      ; FIXME This won't work with cyclic data structures. However, this procedure
      ; is used during compilation with user-defined expressions.
      ;
      (if (pair? expr)
        (let-values (((list tail) (uncons (cdr expr))))
          (values (cons (car expr) list) tail))
        (values '() expr)))

    (define (atom? expr)
      (or (variable? expr) (number? expr) (boolean? expr)))

    (define (tagged-list? expr tag)
      (and (pair? expr) (eq? (car expr) tag)))

    (define (tagged-pair? expr tag)
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
