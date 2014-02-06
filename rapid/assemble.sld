(define-library (rapid assemble)
  (export assemble-program)
  (import (scheme base) (scheme cxr) (scheme write) (rapid base) (rapid srfi 111))
  (begin
  
    (define-record-type variables-record-type
      (make-variables-record counter variables)
      ?variables-record
      (counter variables-record-counter set-variables-record-counter!)
      (variables variables-record-variables set-variables-record-variables!))
  
    (define variables-record
      (make-parameter (make-variables-record 0 '())))

    (define (variable var)
      (if (assq var *ops*)
        var
        (string-append "$"
          (number->string
            (let ((variables (variables-record-variables (variables-record))))
              (cond
                ((assq var variables) => cadr)
                (else
                  (let ((variables (variables-record-variables (variables-record)))
                        (counter (variables-record-counter (variables-record))))
                    (set-variables-record-counter! (variables-record) (+ counter 1))
                    (set-variables-record-variables! (variables-record) `((,var ,counter) . ,variables))
                    counter))))))))
          

    (define (assemble-program program)
      (display "importScripts('stdlib.js');")
      (display "init(new Procedure(function(data){'use strict';")
      (assemble program)
      (display ";}));"))

    (define (assemble expr)
      (cond
        ((number? expr) (assemble-number expr))
        ((variable? expr) (assemble-variable expr))
        ((lambda? expr) (assemble-lambda (cadr expr) (cddr expr)))
        ((set!? expr) (assemble-set! (cadr expr) (caddr expr)))
        ((if? expr) (apply assemble-if (cdr expr)))
        ((op expr) => (lambda (op) (assemble-op op (cdr expr))))
        ((pair? expr) (assemble-application (car expr) (cdr expr)))
        (else (error "assemble: unknown expression type" expr))))

    (define (assemble-number expr)
      (display expr)) ; FIXME
      
    (define (assemble-variable var)
      (display (variable var)))
      
    (define (assemble-lambda args body)
      (display "new Procedure(function(")
      (assemble-args args)
      (display "){")
      (assemble-body body)
      (display "})"))
    
    (define (assemble-set! var expr)
      (assemble var)
      (display "=")
      (assemble expr))
    
    (define (assemble-if pred con alt)
      (display "if((") ; XXX The parentheses may be superfluous.
      (assemble pred)
      (display ")!==false){")
      (assemble con)
      (display "}else{")
      (assemble alt)
      (display "}"))
    
    (define (assemble-op op args)
      (display op)
      (display "(")
      (assemble-args args)
      (display ")"))
    
    (define (assemble-application proc args)
      (cond
        ((lambda? proc)
          (assemble-lambda-application (cadr proc) (cddr proc) args))        
        (else
          ; XXX Shall we handle procedures with no arguments in a special way?
          (display "return [")
          (assemble-args args)
          (display ",")
          (assemble proc)
          (display "]"))))

    ; ((lambda (a b c) x) 1 2 3) --> change to new Procedure... Or something different...
    (define (assemble-lambda-application formals body operands)
      (let loop ((formals formals) (operands operands))
        (unless (null? formals)
          (display "var ")
          (cond
            ((null? operands)
              (assemble (car formals))
              (display ";")
              (loop (cdr formals) '()))
            (else
              (assemble (car formals))
              (display "=")
              (assemble (car operands))
              (display ";")
              (loop (cdr formals) (cdr operands))))))
      (assemble-body body))

    (define (assemble-body body)
      (unless (null? body)
        (assemble (car body))
        (display ";")
        (assemble-body (cdr body))))
              
    (define (assemble-args args)
      (unless (null? args)
        (assemble (car args))
        (let ((rest (cdr args)))
          (unless (null? rest)
            (display ",")
            (assemble-args rest)))))))

