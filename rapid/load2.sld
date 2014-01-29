(define-library (rapid load)
  (export load)
  (import (scheme base) (rapid base))
  (begin
  
    (define-record-type library-table-type
      (make-library-table)
      library-table?
      (libraries library-table-libraries))

    (define-record-type environment-type
      (make-environment)
      environment?
      (frames environment-frames environment-frames-set!))
    
    (define library-table (make-parameter (make-library-table)))

    ; The following procedure is implementation-dependent. It takes a library
    ; name and produces the library source as a datum
    (define (read-library library-name)
      

    (define (import import-set)
      (define (invalid-import-set-error)
        (error "invalid import set" import-set))
      (unless (list? import-set) (invalid-import-set-error))
      (if (and (not (null? (cdr import-set))) (list? (cadr import-set)))
        (case (car import-set)
          ((only) (import (cadr import-set))) ; TODO
          ((except) (import (cadr import-set)))
          ((prefix) (import (cadr import-set)))
          ((rename) (import (cadr import-set)))
          (else (invalid-import-set-error)))
        (begin
          (unless (library-name? import-set)
            (error "invalid library name" import-set))
          (load-library import-set))))

    (define (load program)
      ; at the moment we only allow one import set
      
      (when (null? program)
        (error "import declaration missing"))
      
      (let ((import-declaration (car program)))
        (unless (import-declaration? import-declaration)
          (error "invalid import declaration" import-declaration))
      
        (for-each import (cdr import-declaration)))
        
      (cdr program))
  
      (define (library-name? datum)
        (and (list? datum)
          (and (map (lambda (obj)
                      (or (symbol? obj) (and (exact-integer? obj)
                                          (not (negative? obj)))
                 datum))))))

      (define (import-declaration? datum)
        (and (list? datum) (not (null? datum)) (eq? (car datum) 'import)))))

