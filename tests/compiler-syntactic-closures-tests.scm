(import (scheme base)
	(rapid test)
	(rapid compiler syntax)
	(rapid compiler syntactic-environments)
	(rapid compiler syntactic-closures))

(test-begin "Syntactic Closures")

(test-assert "Allocating syntactic closures yields syntactic closures"
	     (let ((env (make-syntactic-environment)))	     
	       (syntactic-closure? (make-syntactic-closure env '() 'foo))))

(test-assert "Synthetic identifiers are different from existing ones"
	     (not (eq? 'foo (make-synthetic-identifier 'foo))))

(test-eq "Quoting syntactic identifiers yields the underlying symbol"
	 'foo
	 (unclose-form (make-synthetic-identifier 'foo)))

(test-assert "Syntactic closed identifiers are still identifiers"
	     (let ((env (make-syntactic-environment)))
	       (identifier?
		(make-syntactic-closure env '()
					(make-synthetic-identifier 'foo)))))

(test-assert "Closures around non-symbols are not identifiers"
	     (let ((env (make-syntactic-environment)))	       
	       (not (identifier? (make-syntactic-closure env '() '(foo bar))))))

(test-assert "Identifiers with the same meanings"
	     (let ((env (make-syntactic-environment)))
	       (with-syntactic-environment
		env
		(lambda ()
		  (with-scope
		   (lambda ()
		     (identifier=? env 'x (get-syntactic-environment) 'x)))))))

(test-assert "Identifiers with different meanings"
	     (let ((env (make-syntactic-environment)))
	       (with-syntactic-environment
		env
		(lambda ()
		  (with-scope
		   (lambda ()
		     (insert-binding! (datum->syntax 'x) 'foo)
		     (not (identifier=? env 'x (get-syntactic-environment) 'x))))))))

(test-equal "Syntactic closures can be wrapped"
	    'bar
	    (let ((env (make-syntactic-environment)))   ;; problem: w/s-e creates new
	       (with-syntactic-environment
		env
		(lambda ()
		  (insert-binding! (datum->syntax 'x) 'bar)
		  (let ((x (close-syntax 'x (get-syntactic-environment))))
		    (with-scope
		     (lambda ()
		       (insert-binding! (datum->syntax 'x) 'foo)
		       (let ((x (close-syntax x (get-syntactic-environment))))
			 (sc-lookup-denotation! x)))))))))

;; test macro problem with two syntactic closures

(test-end)
