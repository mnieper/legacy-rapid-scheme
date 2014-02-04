(define-library (rapid library)
  (export
    library-name? import-declaration?
    make-export-set export-set-library-name export-set-bindings
    make-binding binding-identifier binding-expression
    make-naming naming-internal naming-external
    make-library library-name globals environment imports exports
    gensym
    import! export! define!
    export-set
    with-new-frame
    library-expression)
  (import
    (scheme base)
    (rapid base)
    (rapid error))
  (begin

    (define (make-export-set library-name bindings)
      (cons library-name bindings))

    (define (export-set-library-name export-set)
      (car export-set))
      
    (define (export-set-bindings export-set)
      (cdr export-set))

    (define (make-binding identifier expression)
      (cons identifier expression))
      
    (define (binding-identifier binding)
      (car binding))
      
    (define (binding-expression binding)
      (cdr binding))
      
    (define (binding-expression-set! binding expression)
      (set-cdr! binding expression))
      
    (define (make-naming internal external)
      (cons internal external))
      
    (define (naming-internal naming)
      (car naming))
      
    (define (naming-external naming)
      (cdr naming))

    (define (make-library library-name gensym)
      `#(,library-name ,gensym () () () ()))

    (define (library-name library)
      (vector-ref library 0))

    (define (gensym library . args)
      (apply (vector-ref library 1) args))

    ; XXX check, e.g. whether this should be called "library-globals".
    (define (globals library)
      (vector-ref library 2))
      
    (define (globals-set! library globals)
      (vector-set! library 2 globals))

    (define (environment library)
      (vector-ref library 3))
      
    (define (environment-set! library environment)
      (vector-set! library 3 environment))
      
    (define (imports library)
      (vector-ref library 4))
      
    (define (imports-set! library imports)
      (vector-set! library 4 imports))
      
    (define (exports library)
      (vector-ref library 5))
      
    (define (exports-set! library exports)
      (vector-set! library 5 exports))

    (define (import! library binding)
      (let ((identifier (binding-identifier binding)) (globals (globals library)))
        (cond
          ((assq identifier globals) =>
            (lambda (global-binding)
              (unless (eq? (binding-expression binding) (binding-expression global-binding))
                (error "identifier imported more than once with different bindings" identifier))))
          (else
            (imports-set! library (cons binding (imports library)))
            (globals-set! library (cons binding globals))))))
        
    (define (export! library export-spec)
      (cond
        ((symbol? export-spec)
          (export! library (list 'rename export-spec export-spec)))
        (else
          (unless
            (and (list? export-spec) (= (length export-spec) 3)
              (eq? (list-ref export-spec 0) 'rename) (symbol? (list-ref export-spec 1))
              (symbol? (list-ref export-spec 2)))
            (error "invalid export specification" export-spec))
          (exports-set! library
            (cons (apply make-naming (cdr export-spec)) 
              (exports library))))))

    (define (outermost-level? library)
      (null? (environment library)))

    (define (define! library binding)
      ;
      ; Create or update a binding in the current frame or in the library's globals.
      ;
      (define identifier (binding-identifier binding))
      (if (outermost-level? library)      
        (let ((globals (globals library)))
          (cond
            ((assq identifier globals) =>
              (lambda (global-binding)
                (binding-expression-set! global-binding (binding-expression binding))))
            (else
              (when (assq identifier (imports library))
                (error "It is an error to redefine or mutate an imported binding" identifier))
              (globals-set! library (cons binding globals)))))
        (let* ((environment (environment library)) (frame (car environment)))
          ; In the alternate, we are inside a body. Maybe check whether we are at the beginning of a body.
          ; or reformulate everything in terms of a letrec* (mutual dependence possible).
          ; Also: how to incorporate define-record-type in a letrec? and define-syntax?
          ; Actually, I don't understand where define-syntax, for example, is allowed...
          ; W.r.t. define-record-type: it seems to be just a couple of defines.
          (cond
            ((assq identifier frame)
              (raise-compile-error "it is an error to define an identifier more than once" identifier)) ; XXX Or to have two equal formals
            (else
              (set-car! environment (cons binding frame)))))))
          
    (define (export-set library)
      (make-export-set (library-name library)
        (let loop ((exports (exports library)))
          (if (null? exports)
            (list)
            (let ((bindings (loop (cdr exports))) (export (car exports)))
              (cond
                ((assq (car export) (globals library)) =>
                  (lambda (global)
                    (cond
                      ((assq (naming-external export) bindings) =>
                        (lambda (binding)
                          (unless (eq? (binding-expression global) (binding-expression binding))
                            (error "exported identifier does not name a single binding" (cadr export)))
                          bindings))
                      (else
                        (cons (make-binding (naming-external export) (binding-expression global)) bindings)))))
                (else
                  (error "exported identifier not bound in library" (car export)))))))))
                  
    (define (library-expression library identifier)
      ; XXX Use library- prefix?
      ;
      (let loop ((environment (environment library)))
        (if (null? environment)
          (cond
            ((assq identifier (globals library)) => binding-expression)
            (else
              (raise-compile-error "identifier not bound" identifier)))
          (cond
            ((assq identifier (car environment)) => binding-expression)
            (else
              (loop (cdr environment)))))))
  
    (define (with-new-frame library thunk)    
      (define frame '())
      (define (before)
        (environment-set! library (cons frame (environment library))))
      (define (after)
        (define env (environment library))
        (set! frame (car env))
        (environment-set! library (cdr env)))
      (dynamic-wind before thunk after))
        
    (define (import-declaration? datum)
      (tagged-list? datum 'import))
                  
    (define (library-name? datum)
      ; Return true if datum is a valid library name.
      ;
      (and (list? datum)
        (and (map (lambda (obj)
                    (or (symbol? obj) (and (exact-integer? obj)
                                        (not (negative? obj)))))
               datum))))))
