(import (rapid primitive))

(define-values (sum)
  (case-lambda
   ((n)  
    (if (fx= n 0)
	(begin
	  (display (ccm))
	  (newline)
	  0)
	(wcm n (fx+ n (sum (fx- n 1))))))))

(define-values (sum2)
  (case-lambda
   ((n a)
    (if (fx= n 0)
	(begin
	  (display (ccm))
	  (newline)
	  a)
	(wcm n (sum2 (fx- n 1) (fx+ n a)))))))

(display
 (sum 10))

(newline)

(display
 (sum2 10 0))

(newline)

