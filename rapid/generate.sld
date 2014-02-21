(define-library (rapid generate)
  (import
    (scheme base)
    (rapid asmjs)
    (rapid module))
  (begin
  
    (define (module)
      (emit-js
        (compile-module
          (let-syntax ((module
                (syntax-rules ()
                  ((module . rest) `(module . rest)))))
            (include "rapid/module.scm")))))
            
            
    (module)
    (newline)
            
            
            ))
