(define-library (rapid compiler table-test)
  (export run-tests)
  (import (scheme base)
	  (rapid test)
	  (rapid compiler table))
  (begin
    (define (run-tests)
      (test-begin "tables")

      ;; TODO
      (test 2 3)

      (test-end))))
