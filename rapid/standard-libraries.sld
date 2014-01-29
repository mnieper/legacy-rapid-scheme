(define-library (rapid standard-libraries)
  (export export-sets)
  (import (scheme base) (rapid library))
  (begin

    ; Some random ideas:
    ;
    ; For normal procedures (that is not macros) we need to turn the arguments
    ; into arguments not using the . notation
    ;
    ; what about apply and case-lambda?
    ;
    ; for example: how are +, *, etc. defined when they are allowed to receive
    ; multiple arguments? -> They get a newly allocated list, I'd say.

    (define (make-operator operator)
      (lambda (context library)   ; TODO a form should have args
        (case context
          ((value)
            `(lambda x y (,operator x y))) ; TODO: fresh names for x and y; number of arguments! We can't necessarily check for the number of arguments at compile-time
                                           ; library needs a gensym
          ((form)
            operator)))) ; FIXME
  
    (define export-sets
      (list
        (make-export-set '(scheme base)
          (list
            (make-binding '+ (make-operator '+))))))))

