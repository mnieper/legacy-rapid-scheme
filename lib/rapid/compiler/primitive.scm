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

;;; TODO: Don't do syntax checks here. Let (rapid), etc., handle those.

;; TODO: We don't need this anymore
(define (make-syntax-expander expander)
  (lambda (syntax)
    (unless (list? (syntax-datum syntax))
      (compile-error "invalid use of syntax as value" syntax))
    (expander syntax)))

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
	(display (syntax-datum (car irritant-syntax*)) port)))       
    (compile-error (get-output-string port) syntax)))

(define begin-expander
  (make-syntax-expander
   (lambda (syntax)
     (expand-into-sequence (cdr (syntax-datum syntax))) syntax)))

(define (unpack-formals formals)
  (let loop ((formals formals) (fixed '()))
    (cond
     ((null? formals)
      (values (reverse fixed) '()))
     ((pair? formals)
      (loop (cdr formals) (cons (car formals) fixed)))
     (else
      (values (reverse fixed) (list formals))))))

(define define-values-expander
  (make-syntax-expander
   (lambda (syntax)
     (define form (syntax-datum syntax))
     (define-values (fixed-variables rest-variable*)
       (unpack-formals (syntax-datum (list-ref form 1))))
     (expand-into-definition fixed-variables
			     (if (null? rest-variable*)
				 #f
				 (car rest-variable*))
			     (list-ref form 1)
			     (list-ref form 2)
			     syntax))))

(define if-expander
  (make-syntax-expander
   (lambda (syntax)
     (define form (syntax-datum syntax))
     (unless (or (= (length form) 2) (= (length form) 3))
       (compile-error "bad if syntax" syntax))
     (let ((test-syntax (list-ref form 1))
	   (consequent-syntax (list-ref form 2))
	   (alternate-syntax (and (= (length form) 3) (list-ref form 3))))
       (expand-into-expression
	(make-conditional
	 (expand-expression test-syntax)
	 (expand-expression consequent-syntax)
	 (if alternate-syntax
	     (expand-expression alternate-syntax)
	     (make-literal #f #f))
	 syntax))))))

(define case-lambda-expander
  (make-syntax-expander
   (lambda (syntax)
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
	     (define formals (expand-formals! (car form)))
	     (make-clause formals (list (expand-body (cdr form) clause-syntax)) clause-syntax))))
	(cdr form))
       syntax)))))

(define (quote-expander syntax)
  (define form (syntax-datum syntax))
  (unless (= (length form) 2)
    (compile-error "bad quote syntax" syntax))
  (expand-into-expression
   (make-literal (syntax->datum (list-ref form 1) unclose-form) syntax)))

(define (auxiliary-syntax)
  (lambda (syntax)
    (compile-error "invalid use of auxiliary syntax" syntax)))

(define (define-syntax-expander syntax)
  (define syntactic-environment (get-syntactic-environment))
  (define form (syntax-datum syntax))
  ;; Form looks as follows:
  ;; (define-syntax <keyword> (syntax-rules <ellipsis> (<literal> ...) <syntax-rule*>))
  (define keyword-syntax (list-ref form 1))
  (define transformer-syntax (list-ref form 2))
  (define transformer (syntax-datum transformer-syntax))
  (define ellipsis-syntax (list-ref transformer 1))
  (define literal-syntax* (syntax-datum (list-ref transformer 2)))
  (define syntax-rule-syntax* (list-tail transformer 3))
  (define macro-environment (get-syntactic-environment))
  (define literal-set
    (let loop ((literal-set (make-set (make-eq-comparator)))
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
  (define ellipsis (syntax-datum ellipsis-syntax))
  (define ellipsis?
    (if (literal? ellipsis)
	(lambda (identifier) #f)
	(lambda (identifier)
	  (eq? identifier ellipsis))))
  (assert-identifier! keyword-syntax)
  (assert-identifier! ellipsis-syntax)
  (let ((transformer (make-syntax-rules-transformer ellipsis?
						    literal?
						    syntax-rule-syntax*
						    transformer-syntax
						    macro-environment)))
    (expand-into-syntax-definition
     keyword-syntax
     (lambda (syntax)
       (expand-syntax! (transformer syntax (get-syntactic-environment))))
     syntax)))     

(define (assert-identifier! syntax)
  (unless (identifier? (syntax-datum syntax))
    (compile-error "bad identifier" syntax)))
  
(define (expand-formals! syntax)
  (define form (syntax-datum syntax))
  (if (or (null? form) (pair? form))  
      (let loop ((form form) (%fixed-arguments '()))
	(cond
	 ((null? form)
	  (make-formals (reverse %fixed-arguments) #f syntax))
	 ((pair? form)
	  (loop (cdr form) (cons (expand-formal! (car form)) %fixed-arguments)))
	 (else
	  (make-formals (reverse %fixed-arguments) (expand-formal! form) syntax))))
      (make-formals '() (expand-formal! syntax) syntax)))

(define (expand-formal! syntax)
  (define form (syntax-datum syntax))
  (assert-identifier! syntax)
  (let ((location (make-location syntax)))
    (insert-binding! syntax location)
    location))

(define (primitive operator)
  (lambda (syntax)
    (define form (syntax-datum syntax))
    (expand-into-expression
     (make-primitive-operation operator (expand-expression* (cdr form)) syntax))))

(define primitive-environment
  (environment
   ;; Bindings
   ()
   ;; Syntactic environment
   (begin begin-expander)
   (define-values define-values-expander)
   (case-lambda case-lambda-expander)
   (if if-expander)
   (quote quote-expander)
   (syntax-error syntax-error-expander)
   (define-syntax define-syntax-expander)
   (syntax-rules (auxiliary-syntax))
   (+ (primitive operator+))
   (apply (primitive operator-apply))
   (display (primitive operator-display)) ;; FIXME: should go into (scheme base)
   (newline (primitive operator-newline)) ;; FIXME: -- "" --
   (string-append (primitive operator-string-append))
   ))
