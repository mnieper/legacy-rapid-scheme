(define-library (rapid expression)
  (export make-expression make-location make-type-of-syntax type-of-syntax?
    expression-location expression-transformer expand-command-or-definition)
  (import
    (scheme base)
    (rapid error)
    (rapid library))
  (begin

    (define (make-expression location transformer)
      (vector location transformer))

    (define (make-location location)
      (make-expression location
        (lambda (library . args)
          `(,location
            ,@(map (lambda (arg) (expand-command-or-definition arg library)) args)))))
      
    (define (make-type-of-syntax transformer)
      (make-expression #f transformer))
  
    (define (expression-location expression)
      (or (vector-ref expression 0)
        (raise-compile-error "invalid use of syntax as value")))
      
    (define (expression-transformer expression)
      (vector-ref expression 1))
  
    (define (type-of-syntax? expression)
      (not (vector-ref expression 0)))
    
    (define (expand-command-or-definition datum library)
      ; TODO Some forms like define are not allowed to occur everywhere or have
      ; different meanings.
      ; Maybe one should better split this into expand-expression and the rest.
      (cond
        ((boolean? datum) datum)
        ((number? datum) datum)
        ((char? datum) datum)
        ((string? datum) datum)
        ((symbol? datum) (expression-location (library-expression library datum)))
        ((list? datum)
          ((expression-transformer (library-expression library (car datum))) library (cdr datum)))
        (else (raise-compile-error "invalid command or definition" datum))))))

