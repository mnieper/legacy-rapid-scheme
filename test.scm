(define-library (test)
  (export (rename + *))
  (import (scheme base))
  (begin
    +
    4 2
    10
    #\space))

(define-library (test2)
  (import (rename (test) (* /))))

(import (test test))

(define f
  (lambda (n)
    (if (= 0 n)
      0
      (+ n (f (- n 1))))))
      
(display (f 2000000))

(display
  (+ 1 (call/cc
       (lambda (k)
         (+ 2 (k 3))))))

