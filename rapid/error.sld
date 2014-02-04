(define-library (rapid error)
  (export raise-compile-error handle-compile-error with-context log)
  (import
    (scheme base)
    (scheme process-context)
    (scheme write)
    (rapid base))
  (begin
    
    ; XXX Ad-hoc procedure.
    (define (log obj)
      (display "*** ")
      (display obj)
      (display " ***")
      (newline))
      
    (define (compile-error? obj)
      (and 
        (not (error-object? obj))
        (not (file-error? obj))
        (not (read-error? obj))
        (tagged-list? obj 'compile-error)))

    (define (make-compile-error message irritants)
      `(compile-error () ,message ,irritants))
    
    (define (compile-error-location compile-error)
      (list-ref compile-error 1))
      
    (define (compile-error-location-set! compile-error location)
      (list-set! compile-error 1 location))
    
    (define (compile-error-message compile-error)
      (list-ref compile-error 2))
      
    (define (compile-error-irritants compile-error)
      (list-ref compile-error 3))
    
    (define (compile-error-with-context! compile-error context)
      (compile-error-location-set! compile-error
        (cons context (compile-error-location compile-error))))
    
    (define-syntax guard-compile-error
      (syntax-rules ()
        ((guard-compile-error (error-object (result1 result2 ...)) . body)
          (guard (error-object
            ((compile-error? error-object) result1 result2 ...)) . body))))
    
    (define-syntax handle-compile-error
      (syntax-rules ()
        ((handle-compile-error . body)
          (guard-compile-error (error-object
              ((compile-error-display error-object)
                (exit #f))) . body))))

    (define-syntax with-context
      (syntax-rules ()
        ((with-context (exp ...) . body) 
          (guard-compile-error (compile-error
              ((compile-error-with-context! compile-error (list exp ...))
                (raise compile-error))) . body))))

    (define (compile-error-display compile-error)
      (parameterize ((current-output-port (current-error-port)))
        (write-string "COMPILE ERROR: ")
        (write-string (compile-error-message compile-error))
        (write-string ":")
        (for-each
          (lambda (irritant)
            (write-string " ")
            (display irritant)) (compile-error-irritants compile-error))
        (newline)
        (for-each 
          (lambda (context)
            (write-string "  ")
            (let loop ((delim ": ") (context context))
              (unless (null? context))
              (display (car context))
              (unless (null? (cdr context))
                (write-string delim)
                (loop " " (cdr context))))
            (newline)) (reverse (compile-error-location compile-error)))))

    (define (raise-compile-error message . irritants)
      (raise (make-compile-error message irritants)))))
