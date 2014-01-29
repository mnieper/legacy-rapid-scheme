(define-library (rapid compile)
  (export compile)
  (import
    (scheme base)
    (rapid base)
    (rapid link)
    (rapid cps)
    (rapid optimize)
    (rapid assemble))
  (begin
  
  
    (define (compile source)
      (assemble-program
        (optimize
          (cps
            (link
              source)
            (lambda (a) `(,a exit))))))))
