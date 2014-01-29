(define-library (rapid test)
  (export test-begin test-end test)
  (import (scheme base) (scheme write))
  (begin
  
    (define (test-begin . o) #f)

    (define (test-end . o) #f)

    (define-syntax test
      (syntax-rules ()
        ((test expected expr)
          (let ((res expr))
            (cond
             ((not (equal? expr expected))
              (display "FAIL: ")
              (write 'expr)
              (display ": expected ")
              (write expected)
              (display " but got ")
              (write res)
              (newline)))))))))

