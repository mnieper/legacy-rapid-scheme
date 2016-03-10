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

(define (lambda-lift expression)
  (parameterize
      ((current-environment (make-environment expression)))
    (let-values (((_ lifted-expression)
		  (lift expression 0)))
      lifted-expression)))

(define (lift expression depth)
  (define syntax (expression-syntax expression))
  (cond
   ((literal? expression)
    (values (current-root)
	    expression))
   ((undefined? expression)
    (values (current-root)
	    expression))
   ((reference? expression)
    (values (lookup-binding-construct (reference-location expression))
	    expression))
   ((procedure-call? expression)
    (let-values (((binding-construct expression*)
		  (lift* (cons (procedure-call-operator expression)
			       (procedure-call-operands expression))
			 (+ depth 1))))
      (values binding-construct
	      (make-procedure-call (car expression*) (cdr expression*) syntax))))
   ((primitive-operation? expression)
    (let-values (((binding-construct expression*)
		  (lift* (primitive-operation-operands expression)
			 (+ depth 1))))
      (values binding-construct
	      (make-primitive-operation (primitive-operation-operator expression)
					expression*
					syntax))))
   ((expression-procedure? expression)
    (lift-procedure expression depth))
   ((assignment? expression)
    (let-values (((binding-construct init)
		  (lift (assignment-expression expression) (+ depth 1))))
      (define location (assignment-location expression))
      (values (binding-construct-deepest binding-construct
					 (lookup-binding-construct location))
	      (make-assignment location init syntax))))
   ((letrec*-expression? expression)
    (lift-letrec*-expression expression depth))
   ((sequence? expression)
    (let-values (((binding-construct expression*)
		  (lift* (sequence-expressions expression) (+ depth 1))))
      (values binding-construct
	      (make-sequence expression* syntax))))
   ((conditional? expression)
    (let-values (((binding-construct expression*)
		  (lift* (list (conditional-test expression)
			       (conditional-consequent expression)
			       (conditional-alternate expression))
			 (+ depth 1))))
      (values binding-construct
	      (make-conditional (list-ref expression* 0)
				(list-ref expression* 1)
				(list-ref expression* 2)
				syntax))))
   (else
    (error "unhandled expression type" expression))))

(define (lift-procedure expression depth)
  (define clauses (procedure-clauses expression))
  (let-values (((binding-construct clause*)
		(do-lift* lift-procedure-clause clauses depth)))
    (let ((procedure (make-procedure
		      clause*
		      (expression-syntax expression))))
      (if (>= (binding-construct-depth binding-construct)
	      (- depth 1))
	  ;; The procedure cannot be lifted further
	  (values binding-construct procedure)
	  ;; The procedure is to be lifted
	  (let ((f (make-location #f)))
	    (insert-binding! binding-construct
			     (make-binding (make-formals (list f) #f)
					   procedure
					   #f))
	    (values binding-construct
		    (make-reference f #f)))))))

(define (lift-procedure-clause clause depth)
  (define formals (clause-formals clause))
  (define locations (formals-locations formals))
  ;; Assertion: The following expression is a letrec*-expression construct.
  (define expression (car (clause-body clause)))
  (for-each
   (lambda (location)
     (insert-location! location expression))
   locations)
  (let-values (((binding-construct lifted-expression)
		(lift expression (+ depth 1))))
    (values binding-construct
	    (make-clause formals
			 (list lifted-expression)
			 (clause-syntax clause)))))

(define (lift-letrec*-expression expression depth)
  (define bindings (letrec*-expression-bindings expression))
  (insert-binding-construct! expression depth)
  (for-each
   (lambda (binding)
     (for-each
      (lambda (location)
	(insert-location! location expression))
      (formals-locations (binding-formals binding))))
   bindings)
  (let-values (((binding-construct1 init*)
		(lift* (map binding-expression bindings) (+ depth 1))))
    (let-values (((binding-construct2 lifted-body)
		  (lift* (letrec*-expression-body expression) (+ depth 2))))
      (values (binding-construct-highest expression
					 (binding-construct-deepest binding-construct1
								    binding-construct2))
	      (make-letrec*-expression
	       (append
		(binding-construct-bindings expression)
		(map
		 (lambda (binding init)
		   (make-binding (binding-formals binding)
				 init
				 (binding-syntax binding)))
		 bindings init*))
	       lifted-body
	       (expression-syntax expression))))))

(define (do-lift* lifter expression* depth)
  (let ((lift*  
	 (map
	  (lambda (expression)
	    (let-values ((lift (lifter expression depth)))
	      lift))
	  expression*)))
    (values (apply binding-construct-deepest (map car lift*))
	    (map cadr lift*))))

(define (lift* expression* depth)
  (do-lift* lift expression* depth))

(define current-environment (make-parameter #f))
(define (make-environment root)
  (let ((binding-construct-table (make-table (make-eq-comparator))))
    (table-set! binding-construct-table root 0)
    (vector binding-construct-table (make-table (make-eq-comparator)) root)))
(define (current-binding-construct-table)
  (vector-ref (current-environment) 0))
(define (current-location-table)
  (vector-ref (current-environment) 1))
(define (current-root)
  (vector-ref (current-environment) 2))
(define (lookup-binding-construct location)
  (table-ref (current-location-table) location))
(define (insert-location! location binding-construct)
  (table-set! (current-location-table) location binding-construct))
(define (insert-binding-construct! binding-construct depth)
  (table-set! (current-binding-construct-table) binding-construct (vector depth '())))
(define (binding-construct-depth binding-construct)
  (vector-ref (table-ref (current-binding-construct-table) binding-construct) 0))
(define (insert-binding! binding-construct binding)
  (let ((datum (table-ref (current-binding-construct-table) binding-construct)))
    (vector-set! datum 1 (cons binding (vector-ref datum 1)))))
(define (binding-construct-bindings binding-construct)
  (vector-ref (table-ref (current-binding-construct-table) binding-construct) 1))

(define binding-construct-deepest
  (case-lambda
   (()
    (current-root))
   ((binding-construct)
    binding-construct)
   ((binding-construct1 binding-construct2)
    (if (>= (binding-construct-depth binding-construct1)
	    (binding-construct-depth binding-construct2))
	binding-construct1
	binding-construct2))
   ((binding-construct . binding-construct*)
    (binding-construct-deepest binding-construct
			       (apply binding-construct-deepest binding-construct*)))))

(define (binding-construct-highest binding-construct1 binding-construct2)
    (if (<= (binding-construct-depth binding-construct1)
	    (binding-construct-depth binding-construct2))
	binding-construct1
	binding-construct2))
