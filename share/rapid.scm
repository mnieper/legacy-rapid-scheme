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

;; XXX: Where do we add fall-through clauses for procedures when called with wrong number of args?

;;; Procedures and Definitions

(define-syntax lambda
  (syntax-rules ()
    ((lambda formals body1 body2 ...)
     (case-lambda
      (formals body1 body2)))
    ((lambda . args)
     (syntax-error "bad lambda form"))))

(define-syntax define
  (syntax-rules ()
    ((define variable expression)
     (define-values (variable) expression))
    ((define (variable . formals) body1 body2 ...)
     (define-values variable
       (lambda formals body1 body2 ...)))
    ((define . args)
     (syntax-error "bad define syntax"))))

;;; Conditionals

(define-syntax else
  (syntax-rules ()
    ((else . _)
     (syntax-error "invalid use of auxiliary syntax ‘else’"))))

(define-syntax =>
  (syntax-rules ()
    ((else . _)
     (syntax-error "invalid use of auxiliary syntax ‘else’"))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
	   (result temp)
	   (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
	   temp
	   (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
	   clause1 clause2 ...)
     (if test
	 (begin result1 result2 ...)
	 (cond clause1 clause2 ...)))))

;;; Binding constructs

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
		      body1 body2 ...)))
	tag)
      val ...))
    ((let . _)
     (syntax-error "bad let syntax"))))

;; TODO: Implement letrec

;;; Control features

(define (values . things)
  (%call-with-current-continuation
   (lambda (cont)
     (apply cont things))))

(define (make-dynamic-point depth in out parent)
  (vector depth in out parent))
(define (dynamic-point-depth point)
  (%vector-ref point 0))
(define (dynamic-point-in point)
  (%vector-ref point 1))
(define (dynamic-point-out point)
  (%vector-ref point 2))
(define (dynamic-point-parent point)
  (%vector-ref point 3))

(define dynamic-point (make-dynamic-point 0 #f #f #f))
(define (get-dynamic-point)
  dynamic-point)
(define (set-dynamic-point! point)
  (set! dynamic-point point))

(define (travel-to-point! here target)
  (cond
   ((%eq? here target)
    (if #f #f))
   ((< (dynamic-point-depth here) (dynamic-point-depth target))
    (travel-to-point! here (dynamic-point-parent target))
    ((dynamic-point-in target)))
   (else
    ((dynamic-point-out here))
    (travel-to-point! (dynamic-point-parent here) target))))

(define (dynamic-wind in body out)
  (in)
  (let ((here (get-dynamic-point)))
    (set-dynamic-point!
     (make-dynamic-point (+ (dynamic-point-depth here) 1)
			 in
			 out
			 here))
    (let-values ((res* (body)))
      (set-dynamic-point! here)
      (out)      
      (%apply values res*))))

(define (call-with-current-continuation proc)
  (%call-with-current-continuation
   (lambda (cont)
     (proc (continuation->procedure cont (get-dynamic-point))))))

(define (continuation->procedure cont point)
  (lambda res*
    (travel-to-point! (get-dynamic-point) point)
    (set-dynamic-point! point)
    (apply cont res*)))

;;; Parameter objects

(define <param-set!> (vector #f))
(define <param-convert> (vector #f))

(define make-parameter
  (case-lambda
   ((init)
    (make-parameter (lambda (value) value)))
   ((init converter)
    (define value (converter init))
    (lambda args
      (cond
       ((null? args)
	value)
       ((eq? (car args) <param-set!>)
	(set! value (cadr args)))
       ((eq? (car args) <param-convert!>)
	converter)
       (else
	(error "bad parameter syntax")))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((param value) ...) body1 body2)
     (parameterize-aux () ((param value) ...) (body1 body2 ...)))
    ((parameterize . args)
     (syntax-error "bad parameterize syntax"))))

(define-syntax parameterize-aux
  (syntax-rules ()
    ((parameterize-aux
	 ((param value p old new) ...)
	 ()
	 body)
     (let ((p param) ...)
       (let ((old (p)) ...
	     (new ((p <param-convert>) value)) ...)
	 (dynamic-wind
	     (lambda () (p <param-set!> new) ...)
	     (lambda () . body)
	     (lambda () (p <param-set!> old) ...)))))
    ((parameterize-aux
      args
      ((param value) . rest)
      body)
     (parameterize-aux
      ((param value p old new) . args)
      rest
      body))))
  
;;; Strings

(define (string? obj)
  (%string? obj))

;;; Vectors

(define (vector? obj)
  (%vector? obj))

(define (vector . element*)
  (list->vector element*))

(define (list->vector element*)
  ;; TODO
  ;; Check if list is circular and count length
  ;; %make-vector
  ;; %vector-set!
  )

;;; Exceptions

(define current-exception-handler
  (make-parameter
   (lambda (condition)
     ;; FIXME: Show condition on the console and exit with #f
     ;; display-error-object???
     )))

(define (with-exception-handler handler thunk)
  (call-with-current-continuation
   (lambda (return)
     (raise     
      (call-with-current-continuation
       (lambda (reraise)     
	 (parameterize
	     ((current-exception-handler
	       (lambda (condition)
		 (handler condition)
		 (reraise (%make-error-object "exception not caught" condition)))))
	   (call-with-values thunk return))))))))
  

;; TODO (raise ...) ... look at some implementation of raise
;; how to implement raise-continuable?
;; by changing the current-exception-handler!


(define (raise obj)
  ((current-exception-handler) obj)
  (raise ",,)"))

(define (error message . obj*)
  (if (%string? message)
      (raise (%make-error-object message obj*))
      (error "not a string" message)))

;;; List procedures

(define (null? obj)
  (%null obj))

(define (cons car cdr)
  (%cons car cdr))

(define (pair? obj)
  (%pair? obj))

(define (car obj)
  (if (pair? obj)
      (%car obj)
      (error "not a pair" obj)))

(define (cdr obj)
  (if (pair? obj)
      (%cdr obj)
      (error "not a pair" obj)))

(define (list list) list)

(define (append list1 list2)
  ;; TODO: More than one argument
  ;; FIXME: Check that first argument is a list
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;; Quasiquotation

(define-syntax unquote
  (syntax-rules ()
    ((unquote . args)
     (syntax-error "invalid use of auxiliary syntax ‘unquote’"))))

(define-syntax unquote-splicing
  (syntax-rules ()
    ((unquote-splicing . args)
     (syntax-error "invalid use of auxiliary syntax ‘unquote-splicing’"))))

(define-syntax quasiquote
  (syntax-rules ()
    ((quasiquote template)
     (quasiquote-aux template))
    ((quasiquote . args)
     (syntax-error "bad quasiquotation"))))

(define-syntax quasiquote-aux
  (syntax-rules (quasiquote unquote unquote-splicing)
    ((quasiquote-aux ,form)
     form)   
    ((quasiquote-aux (,@form . rest))
     (append form (quasiquote rest)))
    ((quasiquote-aux `form . depth)
     (list 'quasiquote (quasiquote-aux form #f . depth)))
    ((quasiquote-aux ,form #f . depth)
     (list 'unquote (quasiquote-aux form . depth)))
    ((quasiquote-aux ,@form x . depth)
     (list 'unquote-splicing (quasiquote-aux form . depth)))
    ((quasiquote-aux (car . cdr) . depth)
     (cons (quasiquote-aux car . depth) (quasiquote-aux cdr . depth)))
    ((quasiquote-aux #(element ...) . depth)
     (list->vector (quasiquote-aux (element ...) . depth)))
    ((quasiquote-aux constant . depth)
     'constant)))
