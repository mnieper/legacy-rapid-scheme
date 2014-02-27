(define-library (rapid codegen)
  (export codegen)
  (import
    (scheme base) (scheme case-lambda)
    (scheme write)
    (rapid scheme) (rapid module))
  (begin

    (define-syntax new-environment
      (syntax-rules ()
        ((new-environment body1 body2 ...)
          (parameterize
            ((environment (make-environment
                  0 '() 0 '() (make-parameter 0) (make-parameter '(0))))) 
              body1 body2 ...))))

    (define-syntax new-frame
      (syntax-rules ()
        ((new-frame body1 body2 ...)
          (parameterize
            (((environment-frame (environment)) (+ (frame) 1)))
            body1 body2 ...))))

    (define-syntax increase-depth
      (syntax-rules ()
        ((increase-depth increase body1 body2 ...)
          (parameterize (((environment-frames (environment))
                (let loop ((frames (frames)))
                  (if (null? frames)
                    '(0)
                     (cons (+ (car frames) increase) (loop (cdr frames)))))))
            body1 body2 ...))))

    (define (codegen cexpr)   ; XXX Is cexpr really a complex expression?
      (new-environment
        (environment-update! cexpr)
        (new-block!
          (lambda (label) `((init) . ,(codegen-expression* (list cexpr)))))  ; TODO: Remove label arg
        (module #| (global-count) |# (blocks))))

    (define (codegen-expression expr)
      (cond
        ((number? expr)
          (codegen-number expr))
        ((boolean? expr)
          (codegen-boolean expr))
        ((string? expr)
          (codegen-string expr))
        ((variable? expr)
          (codegen-variable expr))
        ((set!? expr)
          (codegen-set! (set!-variable expr) (set!-expression expr)))
        ((op expr) =>
          (lambda (op) (codegen-op op (cdr expr))))
        ((if? expr)
          (apply codegen-if (if-test expr) (if-consequent expr) (if-alternate* expr)))
        ((case-lambda? expr)
          (codegen-case-lambda (case-lambda-clauses expr)))
        ((pair? expr)
          (case (car expr)
            ((write-string)
              (codegen-write-string (list-ref expr 1) (list-ref expr 2)))
            (else
              (codegen-application (application-proc expr)
                (application-arg* expr)))))
        (else (error codegen-expression "invalid expression" expr))))

    (define (codegen-expression* arg*)
     (let loop ((arg* arg*))
        (if (null? arg*)
          '()
          (let-values ((code* (codegen-expression (car arg*))))
            (append code* (loop (cdr arg*))))))) 

    (define (codegen-number expr)
      (number expr))

    (define (codegen-boolean expr)
      (boolean-value expr))
      
    (define (codegen-string expr)
      (string-const expr))
      
    (define (codegen-variable var)
      ((location var)))
      
    (define (codegen-set! var expr)
      `(set! ,(codegen-variable var) ,(codegen-expression expr)))
  
    (define (codegen-op op arg*)
      `(,op . ,(codegen-expression* arg*)))

    (define (codegen-if test con . alt*)
      (apply conditional (codegen-expression test)
        `(begin . ,(codegen-expression* (list con)))
        (map (lambda (alt)
          `(begin . ,(codegen-expression* (list alt)))) alt*)))

    (define (codegen-case-lambda clauses)
      (let ((label (new-block! (make-case-lambda-block clauses))))
        `(new-proc ,label env-ptr)))

    (define (make-case-lambda-block clauses)
      (lambda (label)
        (increase-depth 1
          (list
            `(set! aux-reg ,(frame-arg-count 'env-ptr))
            (codegen-case-lambda-clauses clauses)))))

    (define (codegen-case-lambda-clauses clauses)
      (let loop ((clauses clauses))
        (if (null? clauses)
          '(call-error)
          (let ((clause (car clauses)))
            (codegen-case-lambda-clause (case-lambda-clause-formals clause)
              (case-lambda-clause-body clause)
              (loop (cdr clauses)))))))

    (define (codegen-case-lambda-clause formals body else)
      `(if (= aux-reg
          ,(let loop ((formals formals) (i 0))
            (cond
              ((symbol? formals) (error "lists not yet implemented"))
              ((null? formals) i)
              ((pair? formals) (loop (cdr formals) (+ i 1))))))
        (begin . ,(codegen-expression* body)) ,else))
        
    (define (codegen-application proc arg*)
      (apply values
        `((set! frame-ptr (new-frame ,(length arg*)))
          ,@(let loop ((arg* arg*) (i 0))
            (if (null? arg*)
              '()
              `((set! ,(arg 'frame-ptr i) ,(codegen-expression (car arg*)))
                . ,(loop (cdr arg*) (+ i 1))))) .
          ,(cond
            ((case-lambda? proc)
              `((set! env-ptr frame-ptr)
                (set! frame-ptr ,*null-pointer*) .
                ,(let loop ((clauses (case-lambda-clauses proc)))
                  (if (null? clauses)
                    (error codegen-application "wrong number of arguments" proc arg*)
                    (let ((clause (car clauses)))
                      (let loop-formals ((formals (case-lambda-clause-formals clause)) (i 0))
                        (cond
                          ((symbol? formals) (error "lists not yet implemented"))
                          ((null? formals)
                            (if (= i (length arg*))
                              (increase-depth 0
                                (codegen-expression* (case-lambda-clause-body clause)))
                            (loop (cdr clauses))))
                          ((pair? formals)
                            (loop-formals (cdr formals) (+ i 1))))))))))
            (else
              `((set! aux-reg ,(codegen-expression proc))
                (if (not ,(proc-value? 'aux-reg)) (application-error))
                (set! ,(frame-parent-frame 'frame-ptr) ,(proc-frame 'aux-reg))
                (set! env-ptr frame-ptr)
                (set! frame-ptr ,*null-pointer*)
                (set! code-reg ,(proc-label 'aux-reg))
                (break)))))))

    ; XXX This can be done likewise for any other special form.
    ; XXX If a global cannot be found, assume that it is a special.
    (define  (codegen-write-string k string)
      `(write-string ,(codegen-expression k) ,(codegen-expression string)))

    (define (environment-update! expr)
      (cond
        ((set!? expr)
          (environment-update!-set! (set!-variable expr) (set!-expression expr)))
        ((case-lambda? expr)
          (environment-update!-case-lambda (case-lambda-clauses expr)))
        ((if? expr)
          (environment-update!* (cdr expr)))
        ((op expr)
          (environment-update!* (cdr expr)))
        ((pair? expr)
          (environment-update!* expr))))

    (define (environment-update!* expr*)
      (for-each environment-update! expr*))

    (define (environment-update!-set! var expr)
      (if (not (binding var))
        (global-set! var))
      (environment-update! expr))

    (define (environment-update!-case-lambda clauses)
      (new-frame
        (for-each environment-update!-case-lambda-clause clauses)))

    (define (environment-update!-case-lambda-clause clause)
      (let loop ((formals (case-lambda-clause-formals clause)) (i 0))
        (cond
          ((symbol? formals)
            (local-set! formals i))
          ((pair? formals)
            (local-set! (car formals) i)
            (loop (cdr formals) (+ i 1)))))
      (environment-update!* (case-lambda-clause-body clause)))

    (define-record-type <environment>
      (make-environment global-count bindings block-count blocks frame frames)
      environment?
      (global-count environment-global-count environment-global-count-set!)
      (bindings environment-bindings environment-bindings-set!)
      (block-count environment-block-count environment-block-count-set!)
      (blocks environment-blocks environment-blocks-set!)
      (frame environment-frame)
      (frames environment-frames))

    (define environment
      (make-parameter #f))

    (define (frame)
      ((environment-frame (environment))))

    (define (frames)
      ((environment-frames (environment))))

    (define (global-count)
      (environment-global-count (environment)))
      
    (define (blocks)
      (environment-blocks (environment)))

    (define (binding id)
      (assq id (environment-bindings (environment))))

    (define (location id)
      (cdr (or (binding id) (error "identifier not bound" id))))

    (define (global-set! var)
      ; FIXME Remove globals and make this a one-pass code-generator
      (error "Globals are no more allowed.")
      (let ((count (environment-global-count (environment))))
        (environment-bindings-set! (environment)
          (cons
            (cons var (make-global-location count)) 
            (environment-bindings (environment))))
        (environment-global-count-set! (environment) (+ count 1))))

    (define (local-set! var displacement)
      (environment-bindings-set! (environment)
        (cons
          (cons var (make-local-location displacement))
          (environment-bindings (environment)))))

    (define (make-global-location i)
      #f
      #;(let ((name (global i)))
        (lambda () name))) 
    
    (define (make-local-location displacement)
      (let ((frame (frame)))
        (lambda ()
          (local (list-ref (frames) frame) displacement))))

    (define (new-block! make-block)
      (let ((count (environment-block-count (environment))))
        (environment-block-count-set! (environment) (+ count 1))
        (let ((block (make-block count)))        
          (environment-blocks-set! (environment)
            (cons (cons count block) (environment-blocks (environment)))))
        count))

))
