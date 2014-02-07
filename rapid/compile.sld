(define-library (rapid compile)
  (export compile)
  (import
    (scheme base)
    (scheme write) ; XXX
    (rapid base)
    (rapid link)
    (rapid cps)
    (rapid optimize)
    (rapid assemble))
  (begin
    
    (define (compile source)
      (assemble-program
        ;(display ; XXX
        (optimize
          (cps
            `(case-lambda (() . ,(append source '(0))))
            (lambda (a) `(,a exit))))))))
