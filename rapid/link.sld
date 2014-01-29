(define-library (rapid link)
  (export link)
  (import (scheme base) (rapid base) (scheme cxr))
  (begin
  
    (define (link source)
      (let-values
        (((globals body)
          (let loop ((source source))
            (if (null? source)
              (values '() '(0))
              (let-values (((globals body) (loop (cdr source)))
                           ((expr) (car source)))
                (if (define? expr)
                  (let ((var (cadr expr)))
                    (values `(,var . ,globals) `((set! ,var ,(caddr expr)) . ,body)))
                  (values globals `(,expr . ,body))))))))
        `(lambda (,@globals) . ,body)))))
