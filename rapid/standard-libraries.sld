(define-library (rapid standard-libraries)
  (export expand)
  (import
    (scheme base)
    (scheme case-lambda)
    (rapid error)
    (rapid sequence)
    (rapid expression)
    (rapid library))
  (begin

    ; FIND A BETTER FORMAT FOR DESCRIBING LIBRARIES
    ; ONE INCLUDE PER LIBRARY (e.g. one for (scheme base))

    
    
    (define (expand gensym)
      (let-values (((code export-set) (expand-scheme-base gensym)))
        (values code (list export-set))))

    (define (expand-scheme-base gensym)
      (let* ((library (make-library '(scheme base) gensym))
             (body
              (list
                (expand-define-definition library)
                (expand-lambda-definition library)
                (expand-operator-definition library '+ 0 #t)
                (expand-operator-definition library 'apply 1 #t))))
        (export! library 'define)
        (export! library 'lambda)
        (export! library '+)
        (export! library 'apply)
        (values (make-sequence body) (export-set library))))

    ; The Javascript code for + , * , etc. in this library???

    (define (expand-formals library formals)
      (define (new-symbol symbol)
        (let ((new-symbol (gensym library)))
          (define! library (make-binding symbol (make-location new-symbol)))
          new-symbol))
      ;
      ; FIXME In case of cycles in the program, this may loop.
      ;
      (let loop ((formals formals))
        (cond
          ((pair? formals)
            (unless (symbol? (car formals))
              (raise-compile-error "bad argument in formals" (car formals)))
            (cons (new-symbol (car formals)) (loop (cdr formals))))
          ((symbol? formals)
            (new-symbol formals))
          ((null? formals)
            '())
          (else (raise-compile-error "invalid formals" formals))))) ; FIXME formals is just a tail of the original formals
    
    (define (expand-body library body)
      ;
      ; FIXME This does not handle definitions at the begin of a body
      ;
      (map (lambda (datum) (expand-command-or-definition datum library))
        body))

    (define (expand-lambda-definition library)
      (define! library
        (make-binding 'lambda
          (make-type-of-syntax
            (lambda (library args)
              (unless (> (length args) 0)
                (raise-compile-error "not enough input elements" (length args)))
              (let ((formals (car args)) (body (cdr args)))
                (with-new-frame library (lambda ()
                    (define expanded-formals (expand-formals library formals)) ; XXX This has to happen before the body is expanded.
                    (define expanded-body (expand-body library body))
                    `(lambda ,expanded-formals . ,expanded-body))))))))
      (empty-sequence))
    
    (define (expand-define-definition library)
      ;
      ; XXX The following expansion is hard to read. Define a macro language
      ; for describing libraries!
      (define! library
        (make-binding 'define
          (make-type-of-syntax
            (lambda (library args)
              (unless (= (length args) 2)
                (raise-compile-error "wrong number of input elements in define" (length args)))
              (let ((identifier (list-ref args 0)) (expression (list-ref args 1)))
                (define symbol (gensym library))
                (define expanded-expression (expand-command-or-definition expression library))
                (define (redefine!)
                  (define! library (make-binding identifier (make-location symbol)))
                  `(define ,symbol ,expanded-expression))
                (cond
                  ((assq identifier (globals library)) =>
                    (lambda (binding)
                      (let ((global-expression (binding-expression binding)))
                        (if (type-of-syntax? global-expression)
                          (redefine!)
                          `(set! ,(expression-location global-expression) ,expanded-expression)))))
                  (else (redefine!))))))))
      (empty-sequence))




    ; primitive case-lambda implementierung
    
    (define x (case-lambda ....))
    
    Wenn: (x 3 0) oder (x 1) dann x(3,0) oder x(1) in JS.   
    ; dann checke die length durch; einzige Alternative: x ist an mehrere prozeduren gebunden
    ; Alternative: 
    
    
    ; here: len and rest should allow a number of case-lambdas...
    ; so (?) a list of possible formals?
    (define (expand-operator-definition library operator formals-list)
      (define symbol (gensym library))
      `(define ,symbol
        (lambda 
          ; is case-lambda primitive???   <--- 
    
    
    (define (expand-operator-definition library operator len rest?)
      (define symbol (gensym library))
      (define params
        (do ((params '() (cons (gensym library) params))
             (i 0 (+ i 1)))
          ((= i len) params)))
      (define! library
        (make-binding operator
          (make-expression symbol
            (lambda (library args)
              (with-context ("in application of operator" operator)
                (let ((arg-count (length args)))
                  (if rest?
                    (when (< arg-count len)
                      (raise-compile-error "not enough arguments" arg-count))
                    (when (not (= arg-count len))
                      (raise-compile-error "wrong number of arguments" arg-count))))
                `(,operator
                  ,@(map (lambda (arg) (expand-command-or-definition arg library)) args)))))))
      `(define ,symbol
        ,(if rest?
          (let ((rest (gensym library)))
            (if (null? params)          
              `(lambda ,rest (apply ,operator ,rest))
              `(lambda (,@params . ,rest) (apply ,operator ,@params ,rest))))
          `(lambda (,@params) (,operator ,@params)))))


    ))
