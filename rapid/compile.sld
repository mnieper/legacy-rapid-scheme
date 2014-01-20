(define-library (rapid compile)
  (export compile) ; remove link here
  (import
    (scheme base)
    (scheme cxr)
    (rapid base)
    (rapid cps)
    (rapid optimize)
    (rapid assemble))
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
        `(lambda (,@globals) . ,body)))
  
    (define (compile source)
      (assemble-program
        (optimize
          (cps
            (link source)
            (lambda (a) `(,a exit))))))))
