(define-library (rapid compile)
  (export compile) ; remove link here
  (import
    (scheme base)
    (scheme cxr)
    (scheme write) ; XXX
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
        `(case-lambda ((,@globals) . ,body))))

    (define (compile source)
      (define v (gensym))    
      (assemble-program
        ;(display ; XXX
        (optimize
          (cps
            `(case-lambda (() . ,(append source '(#t))))
            ;(link source)
            (lambda (a) `(,a 
                (case-lambda ((,v) (exit ,v)))))))))))

