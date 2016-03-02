(import (rapid primitive)
	(rapid macros))

(display
 (m-expression (m-quote `(1 2 ,(m-cons '3 '4) 5))))
(newline)
