;(import (scheme base))
(import (rapid primitive))

(define-syntax ellipsis (syntax-rules ... ()))

(define-syntax d
  (syntax-rules ... (ellipsis)
    ((d "1" e p t)
     (define-syntax o
       (syntax-rules e ()
	 ((o . p) t))))
    
    ((d "2" e (x ellipsis) t)
     (d "1" e (x ellipsis) t))))   ;; ellipsis" is closed in the 

(d "2" ellipsis (l ellipsis) (l ellipsis))
