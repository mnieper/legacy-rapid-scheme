(define-library (rapid standard-libraries)
  (export export-sets)
  (import (scheme base) (rapid library))
  (begin

    (define (make-operator operator)
      ; FIXME Currently this is a dummy.
      operator)
  
    (define export-sets
      (list
        (make-export-set '(scheme base)
          (list
            (make-binding '+ (make-operator '+))))))))

