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

;;; Expressions

(define-record-type <expression>
  (make-expression type value syntax)
  expression?
  (type expression-type)
  (value expression-value)
  (syntax expression-syntax))

;;; References

(define (make-reference location syntax)
  (make-expression 'reference location syntax))
(define (reference? expression)
  (eq? (expression-type expression) 'reference))
(define (reference-location reference)
  (expression-value reference))

;;; Literals

(define (make-literal datum syntax)
  (make-expression 'literal datum syntax))
(define (literal? expression)
  (eq? (expression-type expression) 'literal))
(define (literal-value expression)
  (expression-value expression))

;;; Undefined values

(define (make-undefined syntax)
  (make-expression 'undefined #f syntax))
(define (undefined? expression)
  (eq? (expression-type expression) 'undefined))

;;; Procedure calls

(define (make-procedure-call operator operand* syntax)
  (make-expression 'procedure-call (vector operator operand*) syntax))
(define (procedure-call? expression)
  (eq? (expression-type expression) 'procedure-call))
(define (procedure-call-operator expression)
  (vector-ref (expression-value expression) 0))
(define (procedure-call-operands expression)
  (vector-ref (expression-value expression) 1))

;;; Primitive operations

(define (make-primitive-operation operator operand* syntax)
  (make-expression 'primitive-operation (vector operator operand*) syntax))
(define (primitive-operation? expression)
  (eq? (expression-type expression) 'primitive-operation))
(define (primitive-operation-operator expression)
  (vector-ref (expression-value expression) 0))
(define (primitive-operation-operands expression)
  (vector-ref (expression-value expression) 1))

;;; Procedures

(define (make-procedure clauses syntax)
  (make-expression 'procedure clauses syntax))
(define (expression-procedure? expression)
  (eq? (expression-type expression) 'procedure))
(define (procedure-clauses procedure)
  (expression-value procedure))
(define-record-type <clause>
  (make-clause formals body syntax)
  clause?
  (formals clause-formals)
  (body clause-body)
  (syntax clause-syntax))

;;; Assignments

(define (make-assignment location expression syntax)
  (make-expression 'assignment (vector location expression) syntax))
(define (assignment? expression)
  (eq? (expression-type expression) 'assignment))
(define (assignment-location assignment)
  (vector-ref (expression-value assignment) 0))
(define (assignment-expression assignment)
  (vector-ref (expression-value assignment) 1))

;;; Letrec* expressions

(define (make-letrec*-expression bindings body syntax)
  (make-expression 'letrec*-expression (vector bindings body) syntax))
(define (letrec*-expression? expression)
  (eq? (expression-type expression) 'letrec*-expression))
(define (letrec*-expression-bindings expression)
  (vector-ref (expression-value expression) 0))
(define (letrec*-expression-body expression)
  (vector-ref (expression-value expression) 1))

;;; Sequencing

(define (make-sequence expressions syntax)
  (make-expression 'sequence expressions syntax))
(define (sequence? expression)
  (eq? (expression-type expression) 'sequence))
(define (sequence-expressions expression)
  (expression-value expression))

;;; Conditionals

(define (make-conditional test consequent alternate syntax)
  (make-expression 'conditional (vector test consequent alternate) syntax))
(define (conditional? expression)
  (eq? (expression-type expression) 'conditional))
(define (conditional-test expression)
  (vector-ref (expression-value expression) 0))
(define (conditional-consequent expression)
  (vector-ref (expression-value expression) 1))
(define (conditional-alternate expression)
  (vector-ref (expression-value expression) 2))

;;; Locations

(define-record-type <location>
  (make-location syntax)
  location?
  (syntax location-syntax))

;;; Bindings

(define-record-type <binding>
  (make-binding formals expression syntax)
  binding?
  (formals binding-formals)
  (expression binding-expression)
  (syntax binding-syntax))

;;; Formals

;; TODO: Rename into ‘parameters’

(define-record-type <formals>
  (%make-formals fixed-arguments rest-argument syntax)
  formals?
  (fixed-arguments formals-fixed-arguments)
  (rest-argument formals-rest-argument)
  (syntax formals-syntax))

(define make-formals
  (case-lambda
   ((fixed-arguments syntax)
    (make-formals fixed-arguments #f syntax))
   ((fixed-arguments rest-argument syntax)
    (%make-formals fixed-arguments rest-argument syntax))))

(define (formals-locations formals)
  (if (formals-rest-argument formals)
      (cons (formals-rest-argument formals) (formals-fixed-arguments formals))
      (formals-fixed-arguments formals)))

;;; Operators

(define-record-type <operator>
  (make-operator identifier) ;; TODO: add compiling instructions, etc.
  operator?
  (identifier operator-identifier))

;;; Expression datums

(define (expression->datum expression)
  (define counter 0)
  (define (gensym prefix)
    (define symbol (string->symbol (string-append prefix
						  "_"
						  (number->string counter))))
    (set! counter (+ counter 1))
    symbol)
  (define identifier-table (make-table (make-eq-comparator)))
  (define (lookup-identifier! location)
    (define syntax (location-syntax location))
    (define prefix
      (if syntax (symbol->string (syntax->datum syntax unclose-form)) "g_"))
    (table-intern! identifier-table location (lambda () (gensym prefix))))
  (define (formals->datum formals)
    (let loop ((fixed-arguments (formals-fixed-arguments formals)))
      (if (null? fixed-arguments)
	  (let ((rest-argument (formals-rest-argument formals)))
	    (if rest-argument
		(lookup-identifier! rest-argument)
		'()))
	  (cons (lookup-identifier! (car fixed-arguments))
		(loop (cdr fixed-arguments))))))
  (let loop ((expression expression))
    (cond
       ;; References
     ((reference? expression)
      (lookup-identifier! (reference-location expression)))
     ;; Literals
     ((literal? expression)
      `(quote ,(literal-value expression)))
     ;; Undefined values
     ((undefined? expression)
      `(if #f #f))
     ;; Procedure calls
     ((procedure-call? expression)
      `(,(loop (procedure-call-operator expression))
	,@(map loop (procedure-call-operands expression))))
     ;; Primitive operations
     ((primitive-operation? expression)
      `(,(operator-identifier (primitive-operation-operator expression))
	,@(map loop (primitive-operation-operands expression))))
     ;; Procedures
     ((expression-procedure? expression)
      `(case-lambda ,@
	(map
	 (lambda (clause)
	   `(,(formals->datum (clause-formals clause))
	     ,@(map loop (clause-body clause))))
	 (procedure-clauses expression))))
     ;; Assignments
     ((assignment? expression)
      `(set! ,(lookup-identifier! (assignment-location expression))
	     ,(loop (assignment-expression expression))))
     ;; Letrec* expressions
     ((letrec*-expression? expression)
      `(letrec*-values ,
	(map
	 (lambda (binding)
	   `(,(formals->datum (binding-formals binding))
	     ,(loop (binding-expression binding))))
	 (letrec*-expression-bindings expression))
	,@(map loop (letrec*-expression-body expression))))
     ;; Sequences
     ((sequence? expression)
      `(begin ,@(map loop (sequence-expressions expression))))
     ;; Conditionals
     ((conditional? expression)
      `(if ,(loop (conditional-test expression))
	   ,(loop (conditional-consequent expression))
	   ,(loop (conditional-alternate expression))))
     (else
      (error "bad expression" expression)))))

;;; Construct a list of bindings

(define-syntax bindings
  (syntax-rules ()
    ((bindings (formals expression) ...)
     (bindings-aux ((formals expression) ...) ()))))

(define-syntax bindings-aux
  (syntax-rules ()
    ((bindings-aux () ((formals expression) ...))
     `(,(make-binding formals expression #f) ...))
    ((bindings-aux (((x ...) expression) binding ...)
		   (converted-binding ...))
     (bindings-aux (binding ...)
		   (converted-binding ... ((make-formals `(,x ...) #f)
					   expression))))
    ((bindings-aux (((x ... . y) expression) binding ...)
		   (converted-binding ...))
     (bindings-aux (binding ...)
		   (converted-binding ... ((make-formals `(,x ...) y #f)
					   expression))))))

;;; Mapping

(define (expression-map transformer expression)
  (cond
   ((reference? expression) expression)
   ((literal? expression) expression)
   ((undefined? expression) expression)
   ((procedure-call? expression)
    (let* ((operator (transformer (procedure-call-operator expression)))
	   (operands (map-in-order transformer (procedure-call-operands
						expression))))
      (make-procedure-call operator
			   operands
			   (expression-syntax expression))))
   ((primitive-operation? expression)
    (let* ((operands (map-in-order transformer
				   (primitive-operation-operands expression))))
      (make-primitive-operation (primitive-operation-operator expression)
				operands
				(expression-syntax expression))))
   ((expression-procedure? expression)
    (let* ((clauses
	    (map-in-order
	     (lambda (clause)
	       (make-clause (clause-formals clause)
			    (map-in-order transformer (clause-body clause))
			    (clause-syntax clause)))
	     (procedure-clauses expression))))
      (make-procedure clauses (expression-syntax expression))))
   ((assignment? expression)
    (let* ((init (transformer (assignment-expression expression))))
      (make-assignment (assignment-location expression)
		       init
		       (expression-syntax expression))))
   ((letrec*-expression? expression)
    (let* ((binding*
	    (map-in-order
	     (lambda (binding)
	       (make-binding (binding-formals binding)
			     (transformer (binding-expression binding))
			     (binding-syntax binding)))
	     (letrec*-expression-bindings expression)))
	   (body (map-in-order transformer
			       (letrec*-expression-body expression))))
      (make-letrec*-expression binding* body (expression-syntax expression))))
   ((sequence? expression)
    (let* ((expression* (map-in-order transformer
				      (sequence-expressions expression))))
      (make-sequence expression* (expression-syntax expression))))
   ((conditional? expression)
    (let* ((test (transformer (conditional-test expression)))
	   (consequent (transformer (conditional-consequent expression)))
	   (alternate (transformer (conditional-alternate expression))))
      (make-conditional test
			consequent
			alternate
			(expression-syntax expression))))
   (else
    (error "unknown expression type" expression))))

(define (expression-for-each visitor expression)
  (begin
    (cond
     ((reference? expression))
     ((literal? expression))
     ((undefined? expression))
     ((procedure-call? expression)
      (visitor (procedure-call-operator expression))
      (for-each visitor (procedure-call-operands expression)))
     ((primitive-operation? expression)
      (for-each visitor (primitive-operation-operands expression)))
     ((expression-procedure? expression)
      (for-each
       (lambda (clause)
	 (for-each visitor (clause-body clause)))
       (procedure-clauses expression)))
     ((assignment? expression)
      (visitor (assignment-expression expression)))
     ((letrec*-expression? expression)
      (for-each
       (lambda (binding)
	 (visitor (binding-expression binding)))
       (letrec*-expression-bindings expression))
      (for-each visitor (letrec*-expression-body expression)))
     ((sequence? expression)
      (for-each visitor (sequence-expressions expression)))
     ((conditional? expression)
      (visitor (conditional-test expression))
      (visitor (conditional-consequent expression))
      (visitor (conditional-alternate expression)))
     (else
      (error "unknown expression type" expression))))
  (if #f #f))
