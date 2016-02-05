(import (scheme base)
	(rapid test)
	(rename (tests rapid test) (run-tests run-test-tests))
	(rename (tests rapid box) (run-tests run-box-tests))
	(rename (tests rapid compiler table) (run-tests run-compiler-table-tests))
	(rename (tests rapid compiler read) (run-tests run-compiler-read-tests)))

(test-begin "libraries")

(run-test-tests)
(run-box-tests)
(run-compiler-table-tests)
(run-compiler-read-tests)

(test-end)
