(define-library (rapid list)
  (export remove)
  (import (scheme base))
  (begin
  
    (define (remove pred list)
      (if (null? list)
        '()
        (let ((obj (car list)) (list (cdr list)))
          (if (pred obj)
            (remove pred list)
            `(,obj . ,(remove pred list))))))))

