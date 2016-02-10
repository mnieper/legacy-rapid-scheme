(import (scheme base)
	(rapid test)
	(rapid compiler map))

(test-begin "Compiler Maps")

(test-assert "Allocating a map yields a map"
	     (map? (make-map (lambda (x) #f) equal?)))

(test-equal "Stored values can be retrieved"
	    (map-lookup (map-insert (make-map (lambda (x) #f) equal?) 1 2) 1)
	    2)

(test-equal "Default values are returned if no value inserted"
	    (map-lookup (map-insert (make-map (lambda (x) (+ x 1)) equal?) 3 4) 5)
	    6)

(test-equal "Stored values can be overwritten"
	    (map-lookup (map-insert
			 (map-insert
			  (make-map (lambda (x) #f) equal?)
			  7 8)
			 7 9)
			7)
	    9)

(test-equal "Keys can be mapped"
	    (map-lookup (map-map-keys
			 (lambda (key) (+ key 1))
			 (map-insert
			  (make-map (lambda (x) #f) equal?)
			  10
			  12))
			11)
	    12)

(test-equal "Maps can be converted into alists"
	    (let ((alist
		   (map->alist
		    (map-insert
		     (map-insert
		      (map-insert
		       (make-map (lambda (x) #f) equal?)
		       13
		       'a)
		      14
		      'b)
		     13
		     'c))))
	      (length alist))
	      2)

;; TODO: map-delete

(test-end)
