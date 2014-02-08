(define-library (rapid assemble)
  (export assemble-program)
  (import (scheme base) (scheme cxr) (scheme write) (rapid base))
  (begin
  
    (define counter 0)
    (define (genvar)
      (set! counter (+ counter 1))
      (string-append "$"
        (number->string counter)))
    
    ; TODO Should be local to assemble-program!
    ; As the counter

    ; with parameterize, this list does not grow too long
    (define variables '())

    ; should also be local
    (define global-counter -1)
    (define (gen-global-var)
      (set! global-counter (+ global-counter 1))
      (indexed-var "$" global-counter)) 

    (define (indexed-var array-var index)
      ;
      ; TODO: Use at more places.
      ;
      (string-append array-var "["
        (number->string index) "]"))
        
    (define (assemble-program program)
      (write-string "'use strict';")
      (write-string "importScripts('stdlib.js');")
      (write-string "init(new rapid.Procedure(function(data){var $=[];")
      (assemble program)
      (write-string "}));"))

    (define (assemble expr)
      (cond
        ((number? expr) (assemble-number expr))
        ((boolean? expr) (assemble-boolean expr))
        ((variable? expr) (assemble-variable expr))
        ((case-lambda? expr) (assemble-case-lambda (cdr expr)))
        ((set!? expr) (assemble-set! (cadr expr) (caddr expr)))
        ((if? expr) (apply assemble-if (cdr expr)))
        ((op expr) => (lambda (op) (assemble-op op (cdr expr))))
        ((pair? expr) (assemble-application (car expr) (cdr expr)))
        (else (error "assemble: unknown expression type" expr))))

    (define (assemble-number expr)
      (display expr)) ; FIXME

    (define (assemble-boolean expr)
      (write-string
        (if expr "rapid.SchemeBoolean.true" "rapid.SchemeBoolean.false")))

    (define (assemble-variable var)
      (write-string
        (cond
          ((assq var variables) => cdr)
          (else
            (let ((gv (gen-global-var)))
              (set! variables (cons (cons var gv) variables))
              gv)))))
              
    (define (assemble-case-lambda clauses)
      (define args-var (genvar))
      (write-string "new rapid.Procedure(function(") ; TODO: We may need the body???
      (write-string args-var)
      (write-string "){")
      (write-string "var n=")
      (write-string args-var)
      (write-string ".length;")
      (let loop ((clauses clauses) (stmt "if"))
        (unless (null? clauses)
          (let ((clause (car clauses)))
            (write-string stmt)
            (assemble-case-lambda-clause (car clause) (cdr clause) args-var)
            (loop (cdr clauses) "else if"))))
      (write-string "else{rapid.callError();}")
      (write-string "})"))
      
    (define (assemble-case-lambda-clause formals body args-var)
      (define rest #f)
      (define n
        (let loop ((formals formals) (i 0))
          (cond
            ((symbol? formals)
              (set! rest (genvar))
              (set! variables (cons (cons formals rest) variables))
              i)
            ((null? formals)
              i)
            ((pair? formals)
              (set! variables (cons (cons (car formals) (string-append args-var "[" (number->string i) "]")) variables))
              (loop (cdr formals) (+ i 1))))))
      (write-string "(n")
      (if rest
        (write-string ">=")
        (write-string "==="))
      (write-string (number->string n))
      (write-string "){")
      (when rest
        (write-string "var ")
        (write-string rest)
        (write-string "=new ArrayList(args,n)")) ; FIXME args -> args-var
      (assemble-body body)
      (write-string "}"))
    
    (define (assemble-set! var expr)
      (assemble var)
      (display "=")
      (assemble expr))

    (define (assemble-if pred con alt)
      (display "if(") 
      (assemble pred)
      (display ".toBoolean()){")
      (assemble con)
      (display "}else{")
      (assemble alt)
      (display "}"))
    
    (define (assemble-op op args)
      (write-string "rapid.")
      (display op)
      (display "(")
      (assemble-args args)
      (display ")"))
    
    (define (assemble-application proc args)
      (cond
        ((case-lambda? proc)
          (assemble-case-lambda-application (cdr proc) args))
        (else
          (write-string "return [")
          (assemble-args args)
          (write-string ",")
          (assemble proc)
          (write-string "];"))))

    (define (assemble-case-lambda-application clauses args)
      (define n (length args))
      (define args-var (genvar))
      (write-string "var ")
      (write-string args-var)
      (write-string "=[")
      (assemble-args args)
      (write-string "];")
      (let loop ((clauses clauses))
        (when (null? clauses)
          (error "wrong number of arguments in procedure call")) ; XXX raise-compile-error
        (let ((clause (car clauses)))
          (let loop2 ((formals (car clause)) (i 0))
            (cond
              ((symbol? formals)
                (let ((rest (genvar)))
                  (write-string "var ")
                  (write-string rest)
                  (write-string "=new ArrayList(")
                  (write-string args-var)
                  (write-string ",")
                  (write-string (number->string i))
                  (write-string ");")
                  (set! variables (cons (cons formals rest) variables))
                  (assemble-body (cdr clause))))
              ((pair? formals)
                (cond
                  ((< i n)
                    (set! variables (cons (cons (car formals) (string-append args-var "[" (number->string i) "]")) variables))
                    (loop2 (cdr formals) (+ i 1)))
                  (else
                    (loop (cdr clauses)))))
              ((null? formals)
                (assemble-body (cdr clause))))))))

    (define (assemble-body body)
      (unless (null? body)
        (assemble (car body))
        (unless (null? (cdr body))
          (write-string ";"))
        (assemble-body (cdr body))))
              
    (define (assemble-args args)
      (unless (null? args)
        (assemble (car args))
        (let ((rest (cdr args)))
          (unless (null? rest)
            (display ",")
            (assemble-args rest)))))))

