(define-library (scheme write)
  (export display)
    
  (define-operator display ((_) (_ _))) ; TODO: case-lambdas for operators!!! TODO: expand libraries

