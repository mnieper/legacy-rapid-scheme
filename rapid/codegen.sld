(define-library (rapid codegen)
  (export codegen)
  (import
    (scheme base) (scheme case-lambda)
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
          (lambda (label) (codegen-expression cexpr)))
        (error (environment-blocks (environment)))))

    (define (codegen-expression expr)
      (cond
        ((number? expr)
          (codegen-number expr))
        ((boolean? expr)
          (codegen-boolean expr))
        ((string? expr)
          (codegen-string expr))
        ((variable? expr?)
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
          (codgen-application (application-proc) (application-arg*)))))

    (define (codegen-expression* arg*)
      (map codegen-expression arg*))  

    (define (codegen-number expr)
      (number expr))

    (define (codegen-boolean expr)
      (boolean expr))
      
    (define (codegen-string expr)
      (string-const expr))
      
    (define (codegen-variable var)
      ((location var)))
      
    (define (codegen-set! var expr)
      `(set! ,(codegen-variable var) ,(codegen-expression expr)))
  
    (define (codegen-op op arg*)
      `(,op . ,(codegen-expression* arg*)))

    (define (codegen-if test con . alt*)
      (apply conditional ,(codegen-expression test) ,(codegen-expression con)
        ,(codegen-expression* alt*))) 

    (define (codegen-case-lambda clauses)
      (let ((label (new-block (make-case-lambda-block clauses))))
        `(procedure ,label env-ptr)))

    (define (make-case-lambda-block clauses)
      (lambda (label)
        (increase-depth 1
          (list
            `(set! aux-reg ,arg-count)
            (codegen-case-lambda-clauses clauses)))))

    (define (codegen-case-lambda-clauses clauses)
      (let loop ((clauses clauses))
        (if (null? clauses)
          '(callError)
          (codegen-case-lambda-clause (case-lambda-clause-formals clause)
            (case-lambda-clause-body clause)
            (loop (cdr clause))))))
            
    (define (codegen-case-lambda-clause formals body else)
      `(if (= auxreg
          ,(let loop ((formals formals) (i 0))
            (cond
              ((symbol? formals) (error "lists not yet implemented"))
              ((null? formals) i)
              ((pair? formals) (loop (cdr formals) (+ i 1))))))
        (begin . ,(codegen-expression* body)) ,else))

    (define (codegen-application proc arg*)
      `((set! frame-ptr (frame ,(length arg*)))
        ,@(let loop ((arg* arg*) (i 0))
          (if (null? arg*)
            '()
            '((set! ,(arg 'frame-ptr i) , (codegen (car arg*)))
              . ,(loop (cdr arg*) (+ i 1)))))
        ; TODO Special case calling of case-lambda literals.
        (set! aux-reg ,(codegen proc))
        (if (not= (& aux-reg #x80000007) ,*proc-tag*) (application-error)) ; *tag-mask*
        (set! aux-reg (& aux-reg #x7ffffff8))  ; *ptr-mask*
        (set! ,(parent-frame-ptr 'frame-ptr) ,(at '(+ aux-reg 4)))  ; proc-parent-frame-ptr
        (set! env-ptr frame-ptr)
        (set! frame-ptr 0)
        (set! code-reg ,(at 'aux-reg))
        (break)))

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
      (if (not (location var))
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
            (local-set! (car formals) i))
            (loop (cdr formals) (+ i 1))))
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

    (define (location id)
      (assq id (environment-bindings (environment))))

    (define (global-set! var)
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
      (let ((name (global i)))
        (lambda () name))) 
    
    (define (make-local-location displacement)
      (let ((frame (frame)))
        (lambda ()
          (local (list-ref (frames) frame) displacement))))

    (define (new-block! make-block)
      (let ((count (environment-block count (environment))))
        (environment-block-count-set! (environment (+ count 1)))
        (let ((block (make-block count)))        
          (environment-blocks-set! (environment)
            (cons (cons count block) (environment-blocks (environment)))))
        count))

))
