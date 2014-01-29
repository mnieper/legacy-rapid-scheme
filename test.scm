(define-library (test blubb))

(define-library (test test)
  (import (test blubb)))


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
