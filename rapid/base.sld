(define-library (rapid base)
  (export make-gensym atom? variable? if? lambda? set!? op define? tagged-list? *ops* tagged-pair?)
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
        
    (define (atom? expr)
      (or (variable? expr) (number? expr)))

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
        '(apply application)
        '(= equality)
        '(+ sum)
        '(- difference)
        '(display display)
        '(exit exit)))))
