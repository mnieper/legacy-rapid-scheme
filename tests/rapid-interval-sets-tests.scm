(import (scheme base)
	(rapid test)
	(rapid interval-sets))

(test-begin "Interval Sets")

(test-assert "Allocating an interval sets yields an interval set"
	     (interval-set? (make-interval-set)))

(test-equal "Containment"
	    '(#f #t)
	    (let*
		((interval-set (make-interval-set))
		 (interval-set (interval-set-insert interval-set 2 10))
		 (interval-set (interval-set-insert interval-set 8 20)))
	      (list (interval-set-contains? interval-set 1)
		    (interval-set-contains? interval-set 11))))

(test-end)
