(define-library (rapid import-set)
  (export import-set)
  (import
    (scheme base)
    (rapid error)
    (rapid list)
    (rapid library))
  (begin

    ; `export-sets' is an association list of pairs mapping a library name to a list of
    ; bindings.
    
    ; `import' is an import set as in 5.2 of R7RS.
    
    (define (import-set import export-sets)
      ; Return a list of bindings corresponding to the import set `import'
      ; where the available exported bindings of libraries are defined by
      ; `export-sets'.
      ;
      ; An error is signaled if `import' is not a valid import set.
      ;
      (unless (list? import)
        (error "invalid import set" import))
        (cond
          ((and (> (length import) 2) (list? (cadr import)))
            (let ((imports (import-set (cadr import) export-sets)))
              (case (car import)
                ((only) (import-set-only imports (cddr import)))
                ((except) (import-set-except imports (cddr import)))
                ((prefix) 
                  (unless (= (length import) 3)
                    (error "invalid import set" import))
                  (import-set-prefix imports (list-ref import 2)))
                ((rename) (import-set-rename imports (cddr import)))
                (else (error "invalid import set" import)))))
          (else
            ; The import set must be of the form <library name>.
            (unless (library-name? import)
              (error "invalid library name" import))
            (cond
              ((assoc import export-sets) =>
                (lambda (export-set) (list-copy (export-set-bindings export-set))))
              (else (raise-compile-error "in import set" "library not found" import))))))
                
    (define (import-set-only imports identifiers)
      (if (null? identifiers)
        '()
        (let ((identifier (car identifiers)))
          (unless (symbol? identifier)
            (error "invalid identifier in import set" identifier))
          (cond
            ((assq identifier imports) =>
              (lambda (binding) (cons binding (import-set-only imports (cdr identifiers)))))
            (else (error "identifier not found in import set" identifier))))))
            
    (define (import-set-except imports identifiers)
      (for-each
        (lambda (identifier) 
          (unless (symbol? identifier)
            (error "invalid identifier in import set" identifier))
          (unless (assq identifier imports)
            (error "identifier not found in import set" identifier)))
        identifiers)
      (let loop ((imports imports))
        (if (null? imports)
          '()
          (let ((import (car imports)))
            (if (memq (car import) identifiers)
              (loop (cdr imports))
              (cons import (loop (cdr imports))))))))

    (define (import-set-prefix imports prefix)
      (unless (symbol? prefix)
        (error "invalid identifier in import set" prefix))
        ; XXX prefix is not a list, it is a symbol and thus has no identity
        ; whatever catches the error does not know where the error comes from
      (let ((prefix-string (symbol->string prefix)))
        (let loop ((imports imports))
          (if (null? imports)
            '()
            (let ((import (car imports)))
              (cons
                (make-binding
                  (string->symbol (string-append prefix-string (symbol->string (car import))))
                  (binding-named import))
                (loop (cdr imports))))))))

    (define (import-set-rename imports renames)
      ; `imports' is a list of bindings.
      ;
      ; `renames' is a list of a lists (<identifier1> <identifier2>).
      ;
      ; TODO The list processing below using apply, append and twice map is
      ; not very efficient.
      ;
      (apply append
        (map
          (lambda (rename)
            (unless
              (and (list? rename) (= (length rename) 2) (symbol? (car rename))
                (symbol? (cadr rename)))
              (error "invalid rename modifier in import set" rename))
            (cond
              ((assq (list-ref rename 0) imports) =>
                (lambda (binding)
                  (make-binding (list-ref rename 1) (binding-named binding))))
              (else
                (error "identifier not found in import set" (car rename)))))
          renames)
        (map
          (lambda (import)
            (if (assq (binding-identifier import) renames)
              (list)
              (list import)))
          imports)))))
