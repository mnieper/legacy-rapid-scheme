(import (rapid primitive))

(define-values (f) (if 1 (case-lambda (() (g))) (case-lambda (() 2))))
(define-values (g) (if 3 (case-lambda (() (f))) (case-lambda (() 4))))
5
