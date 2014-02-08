(define-library (rapid program)
  (export make-program program-gensym)
  (import
    (scheme base)
    (rapid base))
  (begin
  
    (define (make-program)
      (vector (make-gensym)))
      
    (define (program-gensym program)
      (vector-ref program 0))))

