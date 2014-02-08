(define-library (rapid compile)
  (export compile)
  (import
    (scheme base)
    (rapid base)
    (rapid program)
    (rapid cps)
    (rapid link)
    (rapid optimize)
    (rapid assemble))
  (begin

    (define (compile source)
      (let ((program (make-program)))
        (assemble program
          (optimize
            (cps program
              (link source))))))))

