(define-library (rapid resolve)
  (export resolve-import)
  (import (scheme base) (rapid library))
  (begin
  
    (define (resolve-import import-set)
      (unless (list? import-set)
        (error "invalid import set" import-set))
      (cond
        ((and (> (length import-set) 1) (list? (cadr import-set)))
          (case (car import-set)
            ((only) (resolve-import (cadr import-set))) ; TODO
            ((except) (resolve-import (cadr import-set)))
            ((prefix) (resolve-import (cadr import-set)))
            ((rename) (resolve-import (cadr import-set)))
            (else (error "invalid import set" import-set))))
        (else
          (unless (library-name? import-set)
            (error "invalid library name" import-set))
          (cond
            ((library import-set) => library-load)
            (else (error "library not found" import-set))))))))
    
