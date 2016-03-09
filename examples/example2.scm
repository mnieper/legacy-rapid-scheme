(import (rapid primitive))


(define-values (g)
  (case-lambda
   ((k) (k 5))))

(define-values (x)
  (case-lambda
   (()
    (call-with-current-continuation g))))

(set! x 4)


(letrec
    ((g_2
      (case-lambda
       ((g__4 k_5) (k_5 g__4 5)))))
  (letrec
      ((x_0
	(case-lambda
	 ((g__3)
	  (g__3
	   (case-lambda
	    ((g__1)
	     (g_2 g__1 g__1))))))))
    (set! x_0 4)
    #t))
