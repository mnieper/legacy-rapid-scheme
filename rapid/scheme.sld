(define-library (rapid scheme)
  (export variable? if? lambda? set!? op
    set!-expression set!-variable
    case-lambda? case-lambda-clauses case-lambda-clause-formals case-lambda-clause-body)
  
  (import
    (scheme base)
    (only (rapid base) tagged-list?))
  (begin
  
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

    (define (set!-variable form)
      (list-ref form 1))
      
    (define (set!-expression form)
      (list-ref form 2))

    (define (case-lambda-clauses form)
      (cdr form))

    (define (case-lambda-clause-formals clause)
      (car clause))
      
    (define (case-lambda-clause-body clause)
      (cdr clause))

    (define *ops*
      (letrec-syntax ((define-operators
            (syntax-rules ()
              ((_) '())
              ((_ (operator name) definitions ...)
                `((operator . name) . ,(define-operators definitions ...)))
              ((_ operator definitions ...)
                `((operator . operator) . ,(define-operators definitions ...))))))
        (include "rapid/operator.scm")))))
