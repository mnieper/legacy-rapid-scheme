(define-library (tests rapid test)
  (export run-tests)
  (import (scheme base)
	  (rapid test))
  (begin
    (define (run-tests)
      ;; FIXME: srfi-64-test.scm is not a proper body.
      #;(include "tests/lib/tests/rapid/srfi-64-test.scm")
      #f)))
