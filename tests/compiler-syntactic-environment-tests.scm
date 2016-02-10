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

(test-equal "Bindings can be removed"
	    #f
	    (let*
		((syntax1 (string->syntax "d4"))
		 (syntax2 (string->syntax "d5"))
		 (env (make-syntactic-environment))
		 (env (insert-binding syntax1 'd4 env))
		 (env (insert-binding syntax2 'd5 env))
		 (env (delete-binding 'd4 env)))
	      (lookup-denotation 'd4 env)))

(test-equal "Record references"
	    '(#t #f)
	    (let*
		((syntax1 (string->syntax "e1"))
		 (syntax2 (string->syntax "e2"))
		 (env (make-syntactic-environment))
		 (env (insert-binding syntax1 'e1 env))
		 (env (insert-binding syntax2 'e2 env)))
	      (capture-references
	       (lambda ()
		 (lookup-denotation 'e1 env)
		 (values))
	       (lambda ()
		 (list (identifier-referenced? 'e1 env)
		       (identifier-referenced? 'e2 env))))))

(test-equal "Insert individual binding from a another environment"
	    'f1
	    (let*
		((syntax1 (string->syntax "f1"))
		 (syntax2 (string->syntax "f2"))
		 (env0 (make-syntactic-environment))
		 (env1 (make-syntactic-environment))
		 (env0 (insert-binding syntax1 'f1 env0))
		 (env1 (insert-binding-from syntax1 env0 env1 syntax2)))
	      (lookup-denotation 'f2 env1)))

(test-equal "Derive a syntactic environment"
	    '(g1 #t) 
	    (let*
		((g1 (string->syntax "g1"))
		 (g2 (string->syntax "g2"))
		 (env0 (make-syntactic-environment))
		 (env0 (insert-binding g1 'g1 env0))
		 (env1 (derive-syntactic-environment env0
						     g2
						     (lambda (id)
						       (string->symbol
							(string-append
							 "_"
							 (symbol->string id)))))))
	      (list (lookup-denotation '_g1 env1)
		     (eq? (syntax-context (lookup-syntax '_g1 env1)) g2))))

(test-end)
