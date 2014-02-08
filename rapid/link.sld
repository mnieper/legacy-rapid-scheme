(define-library (rapid link)
  (export link)
  (import (scheme base) (rapid base) (scheme cxr))
  (begin
  
    (define (link source)
      `(case-lambda (() . ,(append source '(#t)))))))

