(import (rapid primitive))

(define-values (print)
  (case-lambda
   ((msg) (display msg) (newline))))

(define-syntax m
  (syntax-rules ... (print)
    ;((m "1" a . b) (print '(a b)))
    ;((m "2" a b ... c) (print '(a (b ...) c)))
    ;((m "3" (a b ... c . d)) (print '(a (b ...) c d)))
    ;((m "4" (a b ... c . #(d e))) (print '(a (b ...) c (d e))))
    ((m print) (print "5"))))

;(m "1" 1 2 3)
;(m "2" 1 2 3 4 5)
;(m "3" (1 2 3 4 . 5))
;(m "4" (1 2 3 4 . #(5 6)))
(m print)
