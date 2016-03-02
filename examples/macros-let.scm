(import (rapid macros)
	;(rapid primitive)
	(scheme base)
	(scheme write)
	)

(display
 (m-expression
  (m-quote
   (m-let
    loop (('x '(1 2 3)) ('y ''()))
    (m-if (m-null? 'x)
	  'y
	  (loop (m-cdr 'x) (m-cons '1 'y)))))))

(newline)
  
