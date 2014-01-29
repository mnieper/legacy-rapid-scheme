(define-library (rapid library)
  (export
    library-name?
    make-export-set export-set-library-name export-set-bindings
    make-binding binding-identifier binding-named
    make-naming naming-internal naming-external
    make-library library-name globals environment imports exports
    import! export!
    export-set
    library-named)
  (import (scheme base))
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
      
    (define (make-naming internal external)
      (cons internal external))
      
    (define (naming-internal naming)
      (car naming))
      
    (define (naming-external naming)
      (cdr naming))

    (define (make-library library-name)
      `#(,library-name () () () ()))

    (define (library-name library)
      (vector-ref library 0))

    (define (globals library)
      (vector-ref library 1))
      
    (define (globals-set! library globals)
      (vector-set! library 1 globals))

    (define (environment library)
      (vector-ref library 2))
      
    (define (imports library)
      (vector-ref library 3))
      
    (define (imports-set! library imports)
      (vector-set! library 3 imports))
      
    (define (exports library)
      (vector-ref library 4))
      
    (define (exports-set! library exports)
      (vector-set! library 4 exports))

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
                  
    (define (library-name? datum)
      ; Return true if datum is a valid library name.
      ;
      (and (list? datum)
        (and (map (lambda (obj)
                    (or (symbol? obj) (and (exact-integer? obj)
                                        (not (negative? obj)))))
               datum))))))
