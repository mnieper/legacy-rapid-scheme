(define-library (rapid table)
  (export make-table table? table-ref table-ref/default
	  table-set!
	  table-update!
	  table-intern!)
  (import (scheme base)
	  (scheme case-lambda)
	  (rapid comparators))
  (include "table.scm"))
