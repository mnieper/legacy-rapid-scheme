(define-library (rapid compile)
  (export compile)
  (import
    (scheme base)
    (rapid base)
    (rapid program)
    (rapid cps)
    (rapid link)
    (scheme write) ; XXX
    (rapid optimize)
    (rapid assemble))
  (begin

    (define (compile source)
      (let ((program (make-program)))
        (assemble
          ;(display ; XX
          (optimize
            (cps program
              (link source))) program)))))

