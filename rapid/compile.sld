(define-library (rapid compile)
  (export compile)
  (import
    (scheme base)
    (scheme cxr)
    (scheme write) ; XXX
    (rapid base)
    (rapid cps)
    (rapid optimize)
    (rapid assemble))
  (begin
    
    (define mygensym (make-gensym))

    (define (compile source)
      (define v (mygensym "v")) ; XXX: one counter and environment for all    
      (assemble-program
        ;(display ; XXX
        (optimize
          (cps
            `(case-lambda (() . ,(append source '(#t))))
            (lambda (a) `(,a 
                (case-lambda ((,v) (exit ,v)))))))))))

