(import (rapid primitive))

(define-syntax a
  (syntax-rules ... ()
    ((a
       (G
	 syntax-rule ... ))
     (quote (syntax-rule ...)))))

;;

(a 
  (F
    ((_ 9 keyword)
     10)))

#;(macro 9 X)
