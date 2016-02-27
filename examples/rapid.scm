(import (scheme base))

#;(define-syntax macro
  (syntax-rules ()
    ((macro b c (R L G   ))
     (quote (b c (R L G)) d))))

#;(display (macro b c (1 2 3)))

(define (f x)
  (+ x x))

(display (f 3))

(newline)
