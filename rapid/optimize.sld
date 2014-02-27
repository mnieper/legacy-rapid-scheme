(define-library (rapid optimize)
  (export optimize)
  (import (scheme base))
  (begin
  
    ; TODO Lift functions; accessing globals is very slow at the moment.
  
    (define (optimize program)
      program))) ; TODO: Implement
      
