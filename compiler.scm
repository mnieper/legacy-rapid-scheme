(import
  (scheme base)
  (scheme read)
  (rapid compile))
  
(define (read-program)
  (let ((datum (read)))
    (if (eof-object? datum)
      '()
      (cons datum (read-program)))))
      
(compile (read-program))

