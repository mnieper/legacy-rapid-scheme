(import (scheme base)
	(rapid test)
	(rapid compiler read)
	(rapid compiler syntactic-environment))

(define (string->syntax string)
  (define port (open-input-string string))
  (read-syntax (make-source-port port #f #f) #f))

(test-begin "Compiler Syntactic Environment")

(test-assert "Syntactic environments"
	     (syntactic-environment? (make-syntactic-environment)))

(test-assert "Initial environments are empty"
	     (let ((syntactic-environment (make-syntactic-environment)))
	       (not (lookup-denotation 'a syntactic-environment))))

(test-assert "Identifiers can be bound"
	    (let*
		((syntax (string->syntax "b"))
		 (syntactic-environment
		  (insert-binding syntax
				  1
				  (make-syntactic-environment))))
	      (equal?
	       (list (lookup-denotation 'b syntactic-environment)
		     (lookup-syntax 'b syntactic-environment)
		     (lookup-denotation 'c syntactic-environment))
	       `(1 ,syntax #f))))

(test-equal "Environments can be joined"
	    (let*
		((syntax1 (string->syntax "d1"))
		 (syntax2 (string->syntax "d2"))
		 (env0 (make-syntactic-environment))
		 (env1 (insert-binding syntax1 11 env0))
		 (env2 (insert-binding syntax2 22 env0))
		 (env (insert-bindings-from env2 env1)))
	      (list (lookup-denotation 'd1 env)
		    (lookup-denotation 'd2 env)))
	    '(11 22))

(test-error "Identifiers cannot be rebound in join"
	    (let*
		((syntax1 (string->syntax "d3"))
		 (syntax2 (string->syntax "d3"))
		 (env0 (make-syntactic-environment))
		 (env1 (insert-binding syntax1 33 env0))
		 (env2 (insert-binding syntax2 44 env0))
		 (env (insert-bindings-from env2 env1)))
	      #t))

;; TODO: delete-binding
;; TODO: insert-binding-from
;; TODO: derive-syntactic-environment

(test-end)
