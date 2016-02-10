(import (scheme base) (scheme write)
	(rapid test)
	(rapid compiler read)
	(rapid compiler error))

(define (string->syntax string)
  (define port (open-input-string string))
  (read-syntax (make-source-port port #f #f) #f))

(test-begin "Compiler Error")

(test-assert "Raising an error raises an exception"
	     (guard (condition
		     ((compile-error-object? condition) #t))
		    (compile-error ((string->syntax "error") ("error")))
		    #f))

(test-end)
