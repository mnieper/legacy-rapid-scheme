(import (scheme base)
	(rapid test)
	(rapid comparators)
	(rapid graphs)

	(scheme write))

(test-begin "Graphs")

(test-equal "Fixing Letrec (reloaded) 1"
	    '((t) (g) (s) (r) (f) (q))
	    (let ((graph
		   '((t g s)
		     (g s r)
		     (s r f)
		     (r f q)
		     (f q)
		     (q))))
	      (graph-scc graph (make-eq-comparator))))

(test-equal "Fixing Letrec (reloaded) 2"
	    '((t) (f) (e o))
	    (let ((graph
		   '((t f)
		     (f e)
		     (e o)
		     (o e))))	      
	      (graph-scc graph (make-eq-comparator))))

(test-equal "Fixing Letrec (reloaded) 3"
	    '((t) (f) (y x))
	    (let ((graph
		   '((t f y)
		     (f y x)
		     (y x)
		     (x y))))
	      (graph-scc graph (make-eq-comparator))))

(test-end)
