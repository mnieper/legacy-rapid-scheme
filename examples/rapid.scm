(import (rapid primitive))

(define-syntax M
  (syntax-rules ... ()
    ((a b c (R L G   ))
     (quote (b c (R L G))))))

;;

(display
  (M b c
    (F F2
      ((_ 9 keyword)
       10))))
(newline)

#;(macro 9 X)
