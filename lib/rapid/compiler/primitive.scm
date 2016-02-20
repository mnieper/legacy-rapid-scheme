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

;;; TODO: Don't do syntax checks here. Let (scheme base), etc., handle those.

(define (make-syntax-expander expander)
  (lambda (syntax)
    (unless (list? (syntax-datum syntax))
      (compile-error "invalid use of syntax as value" syntax))
    (expander syntax)))

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
       (make-conditional
	(expand-expression test-syntax)
	(expand-expression consequent-syntax)
	(if alternate-syntax
	    (expand-expression alternate-syntax)
	    (make-literal #f #f))
	syntax)))))

(define case-lambda-expander
  (make-syntax-expander
   (lambda (syntax)
     (define form (syntax-datum syntax))
     (make-procedure
      (map-in-order
       (lambda (clause-syntax)
	 (define form (syntax-datum syntax))
	 (unless (and (not (null? form)) (list? form))
	   (compile-error "bad case-lambda clause" syntax))
	 (with-scope
	  (lambda ()
	    (define formals (expand-formals! (car form)))
	    (make-clause formals (list (expand-body (cdr form) clause-syntax))))))
       (cdr form))))))

(define quote-expander
  (make-syntax-expander
   (lambda (syntax)
     (define form (syntax-datum syntax))
     (unless (= (length form) 2)
       (compile-error "bad quote syntax" syntax))
     (make-literal (syntax->datum (list-ref form 1)) syntax))))

(define (expand-formals! syntax)
  (define form (syntax-datum syntax))
  (let loop ((form form) (%fixed-arguments '()))
    (cond
     ((null? form)
      (make-formals (reverse %fixed-arguments) #f syntax))
     ((pair? form)
      (loop (cdr form) (cons (expand-formal! (car form)) (%fixed-arguments))))
     (else
      (make-formals (reverse %fixed-arguments) (expand-formal! form) syntax)))))

(define (expand-formal! syntax)
  (define form (syntax-datum syntax))
  (unless (identifier? form)
    (compile-error "bad identifier" syntax))
  (let ((location (make-location syntax)))
    (insert-binding! syntax location)
    location))

(define (make-primitive-expander operator)
  (lambda (syntax)
    (define form (syntax-datum syntax))
    (make-primitive-operation operator (expand-expression* (cdr form)) syntax)))

(define primitive-environment
  (environment
   ;; Bindings
   (
    )
   ;; Syntactic environment
   (begin begin-expander)
   (define-values define-values-expander)
   (case-lambda case-lambda-expander)
   (if if-expander)
   (quote quote-expander)
   (+ (make-primitive-expander operator+))
   (apply (make-primitive-expander operator-apply))
   ))
