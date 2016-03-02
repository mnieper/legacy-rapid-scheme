(import (rapid macros)
	(rapid primitive)
	;(scheme base)
	;(scheme write)
	)

(display
 (m-expression
  (m-quote
   (m-let m-loop (('x '(1 2 3 4 5 6 7 8 9 10 11)) ('y '()))
    (m-if (m-null? 'x)
	  'y
	  (m-loop (m-cdr 'x) (m-cons '1 'y)))))))

(newline)

