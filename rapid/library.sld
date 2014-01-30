(define-library (rapid library)
  (export
    library-name? import-declaration?
    make-export-set export-set-library-name export-set-bindings
    make-binding binding-identifier binding-named
    make-naming naming-internal naming-external
    make-library library-name globals environment imports exports
    gensym
    import! export! define!
    export-set
    expand-command-or-definition ; XXX In which library does this belong?
    library-named)
  (import
    (scheme base)
    (rapid base))
  (begin

    (define (make-export-set library-name bindings)
      (cons library-name bindings))

    (define (export-set-library-name export-set)
      (car export-set))
      
    (define (export-set-bindings export-set)
      (cdr export-set))

    (define (make-binding identifier named)
      (cons identifier named))
      
    (define (binding-identifier binding)
      (car binding))
      
    (define (binding-named binding)
      (cdr binding))
      
    (define (binding-named-set! binding named)
      (set-cdr! binding named))
      
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

    (define (globals library)
      (vector-ref library 2))
      
    (define (globals-set! library globals)
      (vector-set! library 2 globals))

    (define (environment library)
      (vector-ref library 3))
      
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
              (unless (eq? (binding-named binding) (binding-named global-binding))
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

    (define (define! library binding)
      (let ((identifier (binding-identifier binding))
            (globals (globals library)))
        (cond
          ((assq identifier globals) =>
            (lambda (global-binding)
              (binding-named-set! global-binding (binding-named binding))))
          (else
            (when (assq identifier (imports library))
              (error "It is an error to redefine or mutate an imported binding" identifier))
            (globals-set! library (cons binding globals))))))
          
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
                          (unless (eq? (binding-named global) (binding-named binding))
                            (error "exported identifier does not name a single binding" (cadr export)))
                          bindings))
                      (else
                        (cons (make-binding (naming-external export) (binding-named global)) bindings)))))
                (else
                  (error "exported identifier not bound in library" (car export)))))))))
                  
    (define (library-named library identifier)
      ; FIXME Check whether the identifier in the current lexical environment
      (cond
        ((assq identifier (globals library)) => binding-named)
        (else
          (error "identifier not bound" identifier))))

    (define (expand-command-or-definition datum library)
      ; TODO Some forms like define are not allowed to occur everywhere.
      ; Maybe one should better split this into expand-expression and the rest.
      (cond
        ((boolean? datum) datum)
        ((number? datum) datum)
        ((char? datum) datum)
        ((string? datum) datum)
        ((symbol? datum) ((library-named library datum) library))
        ((list? datum) ((library-named library (car datum)) library (cdr datum)))
        (else (error "invalid command or definition" datum))))
                  
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
