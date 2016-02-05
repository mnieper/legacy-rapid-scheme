(define-library (rapid compiler read)
  (export make-source-port
	  source-port?
	  read-error-object?
	  read-error-object-message
	  read-error-object-location
	  syntax?
	  syntax-datum
	  syntax-context
	  syntax-source-location
	  source-location-source
	  source-location-start-line
	  source-location-start-column
	  source-location-end-line
	  source-location-end-column
	  syntax->datum
	  read-syntax
	  derive-syntax)
  (import (scheme base)   (scheme write)  ;XXX
	  (scheme char)
	  (scheme case-lambda)
	  (rapid compiler table))
  (include "read.scm"))
