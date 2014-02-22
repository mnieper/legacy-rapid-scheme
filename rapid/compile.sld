(define-library (rapid compile)
  (export compile)
  (import
    (scheme base)
    (rapid base)
    (rapid program)
    (rapid cps)
    (rapid link)
    (rapid optimize)
    (rapid codegen))
  (begin

    (define (compile source)
      (let ((program (make-program)))
        (codegen
          ;(display ; XX
          (optimize
            (cps program
              (link source))))))))

