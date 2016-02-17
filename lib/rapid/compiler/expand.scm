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


(define (make-%binding formals expression syntax) (vector formals expression syntax))
(define (expand-%binding %binding)
  (make-binding
   (vector-ref %binding 0)
   (expand-expression (vector-ref %binding 1))
   (vector-ref %binding 1)))



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

;; problem: the binding will be re-expanded
;; how to handle this? insert-expression! nimmt keine echte expression an!
;;
;; (+ a b) wird zu (+ a b) als expression... (+ a b) als expression wird zu ?
;; woher weiß (+ ...) in welchem Kontext ich bin?
;;
;;
;; expanding: + ...  ;; + has to be bound (eq? car car) ==> true
;;
;; Every macro expands finally into an expression...
;; The only problem are native 

(define (expand-into-expression! expression)  ;; check whether expanding expression... (parameterize cc)
  (define expressions (%get-expressions))
  (if expressions
      (set-expressions! (cons expression expressions))
      (set-bindings! (cons (make-%binding (make-dummy-formals) expression #f))
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
    (set-bindings! (cons (make-%binding formals expression definition-syntax)
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
;; see below
(define (expand-expression syntax)
  (parameterize ((current-context 'expression))
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
       (map expand-expression (get-expressions))
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
    (expand-into-expression! syntax))
   ((null? form)
    (compile-error "empty application in source" syntax))
   ((identifier? form)
    (expand-into-expression! syntax))
   ((list? form)
    (cond
     ((lookup-transformer! (car expression))
      => (lambda (transform!)
	   (transform! syntax)))
      (else
       (insert-expression! syntax))))
   (else
    (compile-error "invalid form" syntax))))

(define (lookup-transformer! syntax)
  (define form (syntax-datum syntax))
  (and
   (identifier? form)
   (let ((denotation (lookup-denotation! form)))
     (and (proc? denotation) denotation))))

(define (expand-expression syntax)
  (define form (syntax-datum syntax))
  (cond
   ((simple-datum? form)
    (make-literal form syntax))
   ((null? form)
    (compile-error "empty application in source" source))
   ((identifier? form)
    (cond
     ((lookup-denotation! form) =>
      (lambda (binding)
	...))
     (else
      (compile-error "undefined variable" syntax))))
   ((list? form)
    
    )
   (else
    (compile-error "invalid form" syntax))))

(define (simple-datum? expression)
  (or (number? expression)
      (boolean? expression)
      (char? expression)
      (string? expression)
      (bytevector? expression)
      (vector? expression)))

(define (identifier? form) (symbol? form)) ;; TODO: syntactic closures

;; BELOW THAT IS OLD CODE

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
