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

;; Expand a top-level body in a given syntactic environment and
;; return two values, a list of generated bindings due to the expansion
;; and the resulting syntactic environment.

;; SHADOW BINDING COLLECT IN EXPAND MACRO?

;; XXX: 1. define-values: how to handle this?
;;         possibilities: letrec*-values / call-with-values / cps-transform
;;      2. do cps-transform already here!
;;      3. Note: define define via define-values

(define (expand body syntactic-environment gensym)
  (let loop ((body body) (bindings '()) (syntactic-environment syntactic-environment))
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
