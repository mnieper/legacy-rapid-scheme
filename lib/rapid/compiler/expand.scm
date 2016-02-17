;;; Rapid Scheme --- An implementation of R7RS

;; Copyright (C) 2016 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define current-bindings (make-parameter #f box))
(define (%get-bindings) (unbox (current-bindings)))
(define (get-bindings) (reverse (%get-bindings)))
(define (set-bindings! bindings) (box-set! (current-bindings) bindings))
(define current-expressions (make-parameter #f box))
(define (%get-expressions) (unbox (current-expressions)))
(define (get-expressions) (reverse (%get-expressions)))
(define (set-expressions! expressions) (box-set! (current-expressions) expressions))
(define current-context (make-parameter 'top-level))

(define (make-deferred-binding formals expression syntax) (vector formals expression syntax))
(define (expand-binding deferred-binding)
  (make-binding
   (vector-ref deferred-binding 0)
   (force (vector-ref deferred-binding 1))
   (vector-ref deferred-binding 1)))

(define (make-dummy-formals)
  (make-formals (list (make-location #f)) #f #f))

(define (make-reference-expander location)
  (lambda (syntax)
    (define form (syntax-datum syntax))
    (if (list? form)
	(delay
	  (make-procedure-call (expand-expression (car form))
			       (expand-expression* (cdr form))
			       syntax))
	(make-reference location syntax))))

(define (expand-into-expression! expression)
  (define expressions (%get-expressions))
  (if expressions
      (set-expressions! (cons expression expressions))
      (set-bindings! (cons (make-binding (make-dummy-formals) expression #f))
		     (%get-bindings))))

(define (insert-location! identifier-syntax)
  (define location (make-location identifier-syntax))
  (insert-binding! identifier-syntax (make-reference-expander location))
  location)

(define (expand-into-definition! fixed-variables
				 rest-variable
				 expression
				 definition-syntax
				 formals-syntax)
  (define expressions (%get-expressions))
  (when (and expressions (not (null? expressions)))
    (compile-error "definitions may not follow expressions in a body" syntax))
  (let*
      ((fixed-locations (map insert-location! fixed-variables))
       (rest-location (if rest-variable (insert-location! rest-variable) #f))
       (formals (make-formals fixed-locations rest-location formals-syntax)))
    (set-bindings! (cons (make-binding formals expression definition-syntax)
			 (%get-bindings)))))

(define (expand-into-syntax-definition! identifier-syntax expander)
  (insert-binding! identifier-syntax expander))

(define (expand-into-sequence! syntax* syntax)
  (if (eq? (current-context) 'expression)
      (for-each expand-syntax syntax*)
      (make-sequence syntax* syntax)))

(define (expand syntax*)
  (parameterize ((current-bindings '()))
    (for-each expand-syntax! syntax*)
    (parameterize ((current-context 'expression))
      (map expand-binding (get-bindings)))))

;; NEEDED? relation to context
(define (expand-expression syntax)
  (parameterize
      ))

(define (expand-body body)
  (define syntax* (syntax-datum body))
  (parameterize ((current-context 'body)
		 (current-expressions '()))
    (for-each expand-syntax! syntax*)
    (when (null? (%get-expressions))
      (compile-error "no expression in body" body))
    (parameterize ((current-context 'expression))
      (make-letrec-expression
       (map expand-binding (get-bindings))
       (map force (get-expressions))
       body))))

(define (expand-syntax! syntax)
  (define (thunk) (%expand-syntax! syntax))
  (if (eq? (current-context) 'top-level)
      (with-isolated-references thunk)
      (thunk)))

(define (%expand-syntax! syntax)
  (define form (syntax-datum syntax))
  (cond
   ((simple-datum? form)
    (insert-expression! (make-expression syntax)))
   ((null? form)
    (compile-error "empty application in source" syntax))
   ((symbol? form)
    (cond
     ((lookup-binding! form) =>
      (lambda (binding)
	(binding-expand! binding syntax)))
     (else (compile-error "undefined variable" syntax))))
   ((list? form)
    (let* ((operator-syntax (car expression))
	   (operator (syntax-datum operator)))
      (if (symbol? operator)
	  (cond
	   ((lookup-binding! operator)
	    => (lambda (binding)
		 (binding-expand! binding syntax)))
	   (else (compile-error "undefined variable" operator-syntax)))
	  (insert-expression! (make-expression syntax)))))
   (else
    (compile-error "invalid form" syntax))))

;; TODO: write a few transformers
;; TODO: write insert-expression!
;; TODO: write make-expression (an expression is itself a thunk!)
    
;; der macro-expander kann verschiedenes machen...
;; etwa add-binding
;;      add-expression
;;      define-value
;;      define-syntax
;;      define-sequence  <-- was dann?

(define (expand-syntax syntax)
  (define form (syntax-datum syntax))
  ;; check whether code to call or expression
  )


(define (simple-datum? expression)
  (or (number? expression)
      (boolean? expression)
      (char? expression)
      (string? expression)
      (bytevector? expression)
      (vector? expression)))

(define (expand-simple-datum? expression-syntax)
  (define expression (syntax-datum expression-syntax))
  (and
   (simple-datum? expression)
   (make-constant expression expression-syntax)))

(define (expand-null? expression-syntax)
  (compile-error "empty application in source" expression-syntax))

(define (expand-identifier? expression-syntax syntactic-environment)
  (define expression (syntax-datum) expression-syntax)
  (and
   (symbol? expression)
   (cond
    ((lookup-binding expression syntactic-environment)
     => (lambda (binding)
	  ((binding-transformer binding) expression-syntax syntactic-environment)))
    (else (compile-error "undefined variable" expression-syntax)))))

(define (expand-combination? expression-syntax syntactic-environment)
  (define expression (syntax-datum expression-syntax))
  (and
   (not (null? expression))
   (list? expression)
   (let* ((operator-syntax (car expression))
	  (operator (syntax-datum operator)))
     (if (symbol? operator)
	 (cond
	  ((lookup-binding operator syntactic-environment)
	   => (lambda (binding)
		((binding-transformer binding) expression-syntax syntactic-environment)))
	  (else (compile-error "undefined variable" operator-syntax)))
	 (make-procedure-call (expand-expression operator-syntax syntactic-environment)
			      (expand-expression* (cdr expression) syntactic-environment)
			      expression-syntax)))))

(define (expand-expression expression-syntax syntactic-environment)
  (define expression (datum->syntax expression-syntax))
  (cond
   ((expand-simple-datum? expression-syntax))
   ((expand-null? expression-syntax))
   ((expand-identifier? expression-syntax syntactic-environment))
   ((expand-combination? expression-syntax syntactic-environment) => only-expression)
   (else (compile-error "invalid form" expression-syntax))))

(define (only-expression expression)
  (cond
   ((definition? expression)
    (compile-error "definition not allowed here"
		   (expression-syntax expression)))
   ((syntax-definition? expression)
    (compile-error "syntax definitions not allowed here"
		   (expression-syntax expression)))
   (else
    expression)))

(define (expand-expression* expression-syntax* syntactic-environment)
  (let loop ((expression-syntax* expression-syntax*))
    (if (null? expression-syntax*)
	'()
	(cons (expand-expression (car expression-syntax*) syntactic-environment)
	      (loop (cdr expression-syntax*))))))

(define (expand-body body syntactic-environment)
  (capture-references
   (lambda ()
     
     
     (lambda () 
     
     expand)
   => return)))

;; was liegt zugrunde?

(define (expand-toplevel-body body syntactic-environment syntax)
  (let loop ((body body)
	     (bindings (make-bindings syntax))
	     (syntactic-environment syntactic-environment))
    (if (null? body)
	(values bindings syntactic-environment) ;;; STOP: We have to expand the right hand sides
	                                        ;;; of the bindings...
	                                        ;;; what about the expressions? (already expanded)
	                                        ;;; alternative: do not expand expressions (?)
	                                        
	(let-values (((bindings syntactic-environment))
		     (expand-toplevel-expression (car body) syntactic-environment))
	  (loop (cdr body) bindings syntactic-environment)))))


;; was liefert dann body zurück? evtl. liste of bindings, syntax-def, expr (to be extended???)
;;
;; so soll zum beispiel (lambda () ...) noch nicht expanded werden...
;; das aktuelle programm expanded (lambda () ...) komplett.
;; wie können wir das verhindern?
;; halbe trafo...

;; XXX: Weitere Frage... wenn alles in capture-references gewrappt...
;; gibt es dann Probleme der Redefinition?
;; was referenced? UUU?

(define (expand-toplevel-expression expression-syntax bindings syntactic-environment)
  (cond
   ((or (expand-simple-datum? expression-syntax)
	(expand-null? expression-syntax)
	(expand-identifier? expression-syntax syntactic-environment))
    => (lambda (expression)
	 (values (bindings-insert ...)
		 syntactic-environment)))
   ((expand-combination? expression-syntax)
    => (lambda (combination)
	 (cond
	  ((definition? ...) ...)
	  ((syntax-definition? ...) ...)
	  ((sequence? ...) ...)
	  (else
	   ;; wieder expression... how to handle procedure-body expansion at this point?
	   ))))))
    
   ((expand-combination? expression-syntax syntactic-environment) => only-expression)
   (else (compile-error "invalid form" expression-syntax))))

;; Expand a top-level body in a given syntactic environment and
;; return two values, a list of generated bindings due to the expansion
;; and the resulting syntactic environment.
(define (expand body syntactic-environment syntax)
  (let loop ((body body)
	     (binding-spec (make-binding-spec syntax))
	     (syntactic-environment syntactic-environment))
    (define (expand-command-or-definition command-or-defintion command-or-definition*)
      (capture-references
       (lambda () (macro-expand command-or-definition syntactic-environment))
       (lambda (syntax)
	 (define form (syntax-datum syntax))
	 (cond
	  ((combination? form)
	   (case (syntax-datum (car form))
	     ((begin)
	      (let-values (((binding-specs syntactic-environment)
			    (loop (cdr form) binding-specs syntactic-environment)))
		(loop command-or-definition binding-specs syntactic-environment)))
	     (else
		...)))))))))

;; Welche speziellen Formen sind zu expandieren?
(begin ...)         ; need to splice this into body
(define ...)        ; add definition to bindings
(define-syntax ...) ;
(include ...)       ; need to splice similarly
(expression ...)    ; add expression (need switch for body)
;;
(+ 1 2)             ; primitive expression
+                   ; call to primitive expression
					; rapid primitive needs bindings and syntactic-environment

					; macro expanders are a bit more than sc-transformers!

;;



    (if (null? body)
	(values binding-spec syntactic-environment)
	(expand-command-or-definition (car body) (cdr body)))))
  
  (let loop ((body body) (bindings '()) (syntactic-environment syntactic-environment))
    (define (expand-body1 body1 body)
      (capture-references
       (lambda ()
	 (macro-expand body1 syntactic-environment gensym))
       (lambda (syntax)
	 (define form (syntax-datum syntax))
	 (cond
	  ((and (not (null? form) (list? form)))
	   (case (syntax-datum (car form))
	     ((begin)
	      (let-values (((bindings syntactic-environment)
			   (loop (cdr form) bindings syntactic-environment)))
		(loop body bindings syntactic-environment)))
	     (else
	      ...)))
	  (else ...)))))
    (if (null? body)
	(values bindings syntactic-environment)
	(expand-body1 (car body) (cdr body)))))

    (if (null? body)
	(values (reverse bindings) syntactic-environment)
	(let ((syntax (macro-expand (car body) syntactic-environment gensym)))
	  (define form (syntax-datum syntax))
	  (define (loop-expression)
	    (define symbol (gensym))
	    (loop (cdr body)
		  (cons (list symbol (expand-expression syntax syntactic-environment gensym))
			bindings)
		  syntactic-environment))
	  (cond
	   ((and (not (null? form) (list? form)))
	    ;; Signal an error on (begin (define begin list)) and (define define 3)
	    ;; for this, we need to collect identifiers that have been modified
	    (case (syntax-datum (car form))
	      ((begin)
	       (loop (append (cdr form) (cdr body))
		     bindings
		     syntactic-environment))
	      ((define) ;; better: define values (?)
	       (unless (= (length form) 3)
		 (compile-error (syntax ("bad define form"))))
	       (cond
		((and-let* ((denotation
			     (lookup-denotation (syntax-datum (cadr form))
						syntactic-environment))
			    ((identifier? denotation))
			    (denotation)))
		 => (lambda (denotation)
		      (define symbol (gensym))
		      (loop (cdr body)
			    (cons (list symbol
					`(set! ,denotation ,(expand-expression (caddr form)
									       syntactic-environment
									       gensym)))
				  bindings)
			    syntactic-environment)))
		(else
		 (let* ((symbol
			 (gensym))
			(new-environment
			 (insert-binding (cadr binding) symbol syntactic-environment)))
		   (loop (cdr body)
			 (cons (list symbol
				     (expand-expression (caddr form)
							new-environment
							gensym))
			       bindings)
			 new-environment)))))
	      (else (loop-expression))))
	   (loop-expression))

	      
	      ))))

;; TODO: expand-body along similar lines

(define (expand-expression expression-syntax syntactic-environment gensym)
  (define syntax (macro-expand expression-syntax syntactic-environment gensym))
  (define datum (syntax-datum syntax))
  (cond
   ((simple-datum? datum)
    datum)
   ((identifier? datum)
    (let ((denotation (lookup-denotation datum syntactic-environment)))
      (unless denotation
	(compile-error (syntax ("undefined variable ‘~a’" datum))))
      (when (proc? denotation)
	(compile-error (syntax ("invalid use of syntax as value"))))
      denotation))
   ((null? datum)
    (compile-error (syntax ("empty application in source"))))
   ((list? datum)
    (let ((keyword (macro-expand (car datum) syntactic-environment gensym)))
      (case keyword
	((quote)
	 (unless (= (length datum) 2)
	   (compile-error syntax ("bad quote form")))
	 (let ((datum (syntax->datum (cadr datum)))) ; XXX: synthetic identifiers handled?
	   (cond
	    ((identifier? datum)
	     (quote-identifier datum))
	    ((self-evaluating? datum) datum)
	    (else `(quote ,datum)))))
	;; TODO: case-lambda
	(else
	 (expand-expression* (cons keyword (cdr datum)) syntactic-environment gensym)))))
   (else
    (compile-error (syntax ("bad expression"))))))

(define (expand-expression* syntax* syntactic-environment gensym)
  (let loop ((syntax* syntax*))
    (if (null? syntax*)
	'()
	(cons (expand-expression (car syntax*) syntactic-environment gensym)
	      (loop (cdr syntax*))))))

(define (macro-expand syntax syntactic-environment gensym)
  (let macro-expand ((syntax syntax))
    (let loop ((syntax syntax))
      (define form (syntax-datum syntax))
      (cond
       ((or (simple-datum? form)
	    (identifier? form)
	    (null? form))
	syntax)
       ((list? form)
	(let ((operator (macro-expand (car form))))
	  (if (identifier? operator)
	      (let ((denotation (lookup-denotation operator syntactiv-environment)))
		(if (proc? denotation)
		    (loop (denotation syntax syntactic-environment gensym))
		    syntax))
	      syntax)))
       (else
	(compile-error (syntax ("bad form"))))))))

(define (simple-datum? form)
  (or (number? form)
      (string? form)
      (boolean? form)
      (bytevector? form)
      (char? form)))

(define (identifier? form)
  (symbol? form))

(define (self-evaluating? form)
  (or (number? form)
      (string? form)
      (boolean? form)
      (bytevector? form)
      (vector? form)))

(define (quote-identifier identifier)
  ;; TODO: synthetic identifiers
  `(quote ,identifier))
