(define-library (rapid output)
  (export output)
  (import
    (scheme base)
    (rapid asmjs))
  (begin
    
    (define (output assembly)
      (parameterize ((current-output-port (open-output-string)))
        (emit-js assembly)
        (get-output-string (current-output-port))))))

