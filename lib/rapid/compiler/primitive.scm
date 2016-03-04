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

;;; Utility functions

(define (assert-identifier! syntax)
  (unless (identifier? (syntax-datum syntax))
    (compile-error "bad identifier" syntax)))

(define (assert-flist! syntax)
  (when (circular-list? syntax)
    (compile-error "circular list in source" syntax)))

(define (unpack-parameters parameters-syntax)
  (define variable-set (make-table (make-eq-comparator)))
  (define (assert-unique-variable! variable-syntax)
    (assert-identifier! variable-syntax)
    (table-update!
     variable-set
     (syntax-datum variable-syntax)
     (lambda (syntax) syntax)
     (lambda () variable-syntax)
     (lambda (syntax)
       (compile-note "previous appearance was here" syntax)
       (compile-error "duplicate parameter" variable-syntax))))
  (define parameters
    (let ((form (syntax-datum parameters-syntax)))
      (if (or (null? form) (pair? form))
	  form
	  parameters-syntax)))
  (assert-flist! parameters)
  (let loop ((parameters parameters) (fixed '()))
    (cond
     ((null? parameters)
      (values (reverse fixed) '()))
     ((pair? parameters)
      (assert-unique-variable! (car parameters))
      (loop (cdr parameters) (cons (car parameters) fixed)))
     (else
      (assert-unique-variable! parameters)
      (values (reverse fixed) (list parameters))))))

(define (expand-parameters! parameters-syntax)
  (define-values (fixed rest*)
    (unpack-parameters parameters-syntax))
  (make-formals
   (map expand-parameter! fixed)
   (if (null? rest*)
       #f
       (expand-parameter! (car rest*)))
   parameters-syntax))
  
(define (expand-parameter! syntax)
  (let ((location (make-location syntax)))
    (insert-binding! syntax location)
    location))

(define (make-auxiliary-syntax identifier)
  (lambda (syntax)
    (compile-error (format "invalid use of auxiliary syntax ‘~a’"
			   identifier)
		   syntax)))

(define (primitive operator)
  (lambda (syntax)
    (define form (syntax-datum syntax))
    (expand-into-expression
     (make-primitive-operation operator (expand-expression* (cdr form)) syntax))))

;;; Expanders

(define (define-values-expander syntax)
  (define form
    (let ((datum (syntax-datum syntax)))
      (unless (= (length datum) 3)
	(compile-error "bad define-values syntax" syntax))
      datum))
  (define-values (fixed-variables rest-variable*)
    (unpack-parameters (list-ref form 1)))
  (expand-into-definition fixed-variables
			  (if (null? rest-variable*)
			      #f
			      (car rest-variable*))
			  (list-ref form 1)
			  (list-ref form 2)
			  syntax))

(define (syntax-error-expander syntax)
  (define form (syntax-datum syntax))
  (define message (syntax-datum (cadr form)))
  (unless (string? message)
    (compile-error "not a string literal" (cadr form)))
  (let ((port (open-output-string)))
    (display message port)
    (when (> (length form) 2)
      (display ":" port)
      (do ((irritant-syntax* (cddr form) (cdr irritant-syntax*)))
	  ((null? irritant-syntax*))
	(display " " port)
	(display (syntax->datum (car irritant-syntax*) unclose-form) port)))       
    (compile-error (get-output-string port) syntax)))

(define (begin-expander syntax)
  (expand-into-sequence (cdr (syntax-datum syntax)) syntax))

(define (set!-expander syntax)
  (define form
    (let ((datum (syntax-datum syntax)))
      (unless (and (= (length datum) 3)
		   (identifier? (list-ref datum 1)))
	(compile-error "bad set! syntax"))
      datum))
  (define location
    (let* ((identifier-syntax (list-ref datum 1))
	   (identifier (syntax-datum identifier-syntax))
	   (denotation (sc-lookup-denotation! identifier)))
      (unless denotation
	(compile-error (format "identifier ‘~a’ is not bound"
			       (unclose-form identifier))
		       identifier-syntax))
      (when (procedure? denotation)
	(compile-note (format "identifier ‘~a’ was bound here" (unclose-form identifier))
		      (sc-lookup-syntax! identifier))
	(compile-error (format "invalid use of syntax ‘~a’ as value"
			       (unclose-form identifier))
		       identifier-syntax))
      denotation))
  (expand-into-expression
   (make-assignment
    (make-location location identifier-syntax)
    (expand-expression (list-ref form 2))
    syntax)))

(define (if-expander syntax)
  (define form (syntax-datum syntax))
  (unless (or (= (length form) 3) (= (length form) 4))
    (compile-error "bad if syntax" syntax))
  (let ((test-syntax (list-ref form 1))
	(consequent-syntax (list-ref form 2))
	(alternate-syntax (and (= (length form) 4) (list-ref form 3))))
    (expand-into-expression
     (make-conditional
      (expand-expression test-syntax)
      (expand-expression consequent-syntax)
      (if alternate-syntax
	  (expand-expression alternate-syntax)
	  (make-undefined syntax))
      syntax))))

(define (case-lambda-expander syntax)
  (define form (syntax-datum syntax))
  (expand-into-expression
   (make-procedure
    (map-in-order
     (lambda (clause-syntax)
       (define form (syntax-datum clause-syntax))
       (unless (and (not (null? form)) (list? form))
	 (compile-error "bad case-lambda clause" clause-syntax))
       (with-scope
	(lambda ()
	  (define parameters (expand-parameters! (car form)))
	  (make-clause parameters (list (expand-body (cdr form) clause-syntax)) clause-syntax))))
     (cdr form))
    syntax)))

(define (quote-expander syntax)
  (define form (syntax-datum syntax))
  (unless (= (length form) 2)
    (compile-error "bad quote syntax" syntax))
  (expand-into-expression
   (make-literal (syntax->datum (list-ref form 1) unclose-form) syntax)))

(define syntax-rules-expander (make-auxiliary-syntax 'syntax-rules))
(define ellipsis-expander (make-auxiliary-syntax '...))
(define underscore-expander (make-auxiliary-syntax '_))

(define (define-syntax-expander syntax)
  (define form
    (let ((datum (syntax-datum syntax)))
      (unless (= (length datum) 3)
	(compile-error "bad define-syntax syntax" syntax))
      datum))
  (define keyword-syntax
    (let ((syntax (list-ref form 1)))
      (assert-identifier! syntax)
      syntax))
  (define transformer-syntax (list-ref form 2))
  (define-values (ellipsis-syntax literal-syntax* syntax-rule-syntax*)
    (let ((transformer (syntax-datum transformer-syntax)))
      (unless (and (not (null? transformer)) (list? transformer))
	(compile-error "bad transformer spec" transformer-syntax))
      (unless (eq? (sc-lookup-denotation! (syntax-datum (car transformer)))
		   syntax-rules-expander)
	(compile-error "unknown transformer spec" transformer-syntax))
      (cond
       ((and (>= (length transformer) 2)
	     (list? (syntax-datum (list-ref transformer 1))))
	(values #f
		(syntax-datum (list-ref transformer 1))
		(list-tail transformer 2)))
       ((and (>= (length transformer) 3)
	     (identifier? (syntax-datum (list-ref transformer 1)))
	     (list? (syntax-datum (list-ref transformer 2))))
	(values (list-ref transformer 1)
		(syntax-datum (list-ref transformer 2))
		(list-tail transformer 3)))
       (else
	(compile-error "bad syntax-rules syntax" transformer-syntax)))))
  (define ellipsis (if ellipsis-syntax (syntax-datum ellipsis-syntax) #f))
  (define macro-identifier=?
    (with-scope
     (lambda ()
       (define ellipsis-environment (get-syntactic-environment))
       (when ellipsis-syntax
	 (insert-binding! ellipsis-syntax (make-denotation ellipsis-syntax)))
       (lambda (identifier1 identifier2)
	 (identifier=? ellipsis-environment identifier1
		       ellipsis-environment identifier2)))))
  (define macro-environment (get-syntactic-environment))
  (define identifier-comparator
    (make-comparator identifier? macro-identifier=? #f #f))
  (define literal-set
    (let loop ((literal-set (make-set identifier-comparator))
	       (literal-syntax* literal-syntax*))
      (if (null? literal-syntax*)
	  literal-set
	  (let* ((literal-syntax (car literal-syntax*))
		 (literal (syntax-datum literal-syntax)))
	    (assert-identifier! literal-syntax)
	    (when (set-contains? literal-set literal)
	      (compile-error "duplicate literal identifier" literal-syntax))
	    (loop (set-adjoin literal-set literal) (cdr literal-syntax*))))))
  (define (literal? identifier)
    (set-contains? literal-set identifier))
  (define ellipsis?
    (lambda (form)
      (and (identifier? form)
	   (not (literal? form))
	   (if ellipsis
	       (macro-identifier=? form ellipsis)
	       (eq? (sc-lookup-denotation! form) ellipsis-expander)))))
  (define (underscore? identifier)
    (eq? (sc-lookup-denotation! identifier) underscore-expander))
  (define transformer
    (make-syntax-rules-transformer ellipsis?
				   literal?
				   underscore?
				   syntax-rule-syntax*
				   transformer-syntax
				   macro-environment))
  (expand-into-syntax-definition
   keyword-syntax
   (lambda (syntax)
     (expand-syntax! (transformer syntax (get-syntactic-environment))))
   syntax))

;;; Primitive environment of (rapid primitive)

(define primitive-environment
  (environment
   ;; Bindings
   ()

   ;; Syntactic environment

   ;; Literal expressions
   (quote quote-expander)
   ;; Procedures
   (case-lambda case-lambda-expander)
   ;; Conditionals
   (if if-expander)
   ;; Assignments
   (set! set!-expander)
   ;; Inclusion
   ;; TODO
   #;(include include-expander)
   #;(include-ci incude-ci-expander)
   ;; Sequencing
   (begin begin-expander)
   ;; Macros
   (syntax-rules syntax-rules-expander)
   (... ellipsis-expander)
   (_ underscore-expander)
   (syntax-error syntax-error-expander)
   ;; Variable definitions
   (define-values define-values-expander)
   ;; Syntax definitions
   (define-syntax define-syntax-expander)
   ;; Record-type definitions
   ;; TODO
   ;; Equivalence predicates
   (eq? (primitive operator-eq?))
   ;; Numbers
   (fixnum? (primitive operator-fixnum?))
   (flonum? (primitive operator-flonum?))
   (exact? (primitive operator-exact?))
   (nan? (primitive operator-nan?))
   (fx+ (primitive operator-fx+))
   (fx< (primitive operator-fx<))
   (fxnegative? (primitive operator-fxnegative?))
   ;; TODO
   ;; Strings
   (string? (primitive operator-string?))
   ;; TODO
   ;; Vectors
   (make-vector (primitive operator-make-vector))
   (vector-ref (primitive operator-vector-ref))
   (vector-set! (primitive operator-vector-set!))
   (vector? (primitive operator-vector?))
   ;; Control features
   (call-with-current-continuation (primitive operator-call-with-current-continuation))
   ;; TODO
   #; (call-with-values (primitive operator-call-with-values))
   
   (cons (primitive operator-cons))
   (car (primitive operator-car))
   (cdr (primitive operator-cdr))
   (pair? (primitive operator-pair?))
   (null? (primitive operator-null?))
   (make-vector (primitive operator-make-vector))
   (+ (primitive operator+))
   (apply (primitive operator-apply))
   (display (primitive operator-display)) ;; FIXME: should go into (scheme base)
   (newline (primitive operator-newline)) ;; FIXME: -- "" --
   (string-append (primitive operator-string-append))
   ))
