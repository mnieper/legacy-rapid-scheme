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

;;; TODO: Check whether number of values in continuation and set-values!
;;; is always the correct one. 

;;; TODO: Write/improve convenience functions (like make-continuation-procedure)

(define (cps-transform expression)
  (transform expression (lambda (a) a)))

#;(let ((c (make-location #f)))
    (make-procedure
     (list
      (make-clause
       (make-formals (list c) #f)
       (list
	(parameterize ((current-continuation (make-reference c #f)))
	  (%cps-transform expression)))
       #f))
     #f))

(define (generate-procedure fixed-parameters rest-parameter body)
  (%generate-procedure
   (make-clause (make-formals fixed-parameters rest-parameter #f) body #f)))

(define (%generate-procedure clause)
  (let ((f (make-location #f)))
    (make-letrec-expression
     (list
      (make-binding (make-formals (list f) #f)  
		    (make-procedure (list clause) #f)
		    #f))
     (list (make-reference f #f))
     #f)))

(define (continuation-procedure k)
  (if (procedure? k)
      k
      (lambda e* (make-procedure-call k e* #f))))

(define (continuation-expression k)
  (if (procedure? k)
      (let ((c (make-location #f)))
	(let-values ((body (k (make-reference c #f))))
	  (generate-procedure (list c) #f body)))
      k))

(define (atomic? expression)
  (or (reference? expression)
      (literal? expression)
      (undefined? expression)))
  
(define (transform expression k)
  (cond
   ((atomic? expression)
    ((continuation-procedure k) expression))
   ((procedure-call? expression)
    (transform-procedure-call expression k))
   ((primitive-operation? expression)
    (case (operator-identifier (primitive-operation-operator expression))
      ((call-with-current-continuation)
       (transform-call/cc expression k))
      ((apply)
       (transform-apply expression k))
      (else
       (transform-primitive-operation expression k))))
   ((expression-procedure? expression)
    (transform-procedure expression k))
   ((assignment? expression)
    (transform-assignment expression k))
   ((multiple-assignment? expression)
    (transform-multiple-assignment expression k))
   ((letrec-expression? expression)
    (transform-letrec-expression expression k))
   ((let-values-expression? expression)
    (transform-let-values-expression expression k))
   ((sequence? expression)
    (transform-sequence expression k))
   ((conditional? expression)
    (transform-conditional expression k))
   (else
    (error "unhandled expression type" expression))))

(define (transform-procedure-call expression k)
  (transform* (cons (procedure-call-operator expression)
		    (procedure-call-operands expression))
	      (lambda (t*)
		(make-procedure-call (car t*)
				     (cons (continuation-expression k)
					   (cdr t*))
				     (expression-syntax expression)))))

(define (transform-apply expression k)
  (define operands (primitive-operation-operands expression))
  (transform* operands
	      (lambda (a*)
		(make-primitive-operation
		 (primitive-operation-operator expression)
		 (append
		  (list (car a*) (continuation-expression k))
		  (cdr a*))
		 (expression-syntax expression)))))

(define (transform-primitive-operation expression k)
  (transform* (primitive-operation-operands expression)
	      (lambda (t*)
		((continuation-procedure k)
		 (make-primitive-operation (primitive-operation-operator expression)
					   t*
					   (expression-syntax expression))))))

(define (transform-assignment expression k)
  (transform (assignment-expression expression)
	     (lambda (t)
	       ((continuation-procedure k)
		(make-assignment (assignment-location expression)
				 t
				 (expression-syntax expression))))))

(define (transform-multiple-assignment expression k)
  (define formals (multiple-assignment-formals expression))
  (define new-formals
    (make-formals
     (map (lambda (argument) (make-location #f)) (formals-fixed-arguments formals))
     (and (formals-rest-argument formals)
	  (make-location #f))
     #f))
  (transform (multiple-assignment-expression expression)
	     (%generate-procedure
	      (make-clause
	       new-formals
	       (append
		(map
		 (lambda (argument new-argument)
		   (make-assignment argument (make-reference new-argument #f) #f))
		 (formals-fixed-arguments formals)
		 (formals-fixed-arguments new-formals))
		(if (formals-rest-argument formals)
		    (list
		     (make-assignment (formals-rest-argument formals)
				      (make-reference
				       (formals-rest-argument new-formals)
				       #f)
				      #f))
		    '())
		(list
		 (make-procedure-call (continuation-expression k)
				      (list (make-undefined #f)))))))))

#;(define (transform-call/cc expression k)
  (define operand (car (primitive-operation-operands expression)))
  (let ((c (make-location #f)))
    ((continuation-procedure k)
     (make-continuation-procedure (list c)
				  (list (transform operand
						   (lambda (a)
						     (make-procedure-call
						      a
						      (list (make-reference c #f)
							    (make-reference c #f))
						      (expression-syntax expression)))))))))

;; TODO: Check whether the following is too complicated
;; and whether we can use the above!

(define (transform-call/cc expression k)
  (transform (car (primitive-operation-operands expression))
	     (lambda (a)	       
	       (let ((c (make-location #f)))
		 (make-procedure-call
		  (generate-procedure
		   (list c)
		   #f
		   (list
		    (make-procedure-call a
					 (list (make-reference c #f)
					       (let ((_ (make-location #f))
						     (x* (make-location #f)))
						 (generate-procedure
						  (list _) x*
						  (list
						   (make-primitive-operation
						    operator-apply
						    (list
						     (make-reference c #f)
						     (make-reference x* #f))
						    #f)))))
					 (expression-syntax expression))))
		  (list (continuation-expression k))
		  #f)))))

(define (transform-procedure expression k)
  (do-transform* transform-procedure-clause
		 (procedure-clauses expression)
		 (lambda (c*)
		   ((continuation-procedure k)
		    (make-procedure c* (expression-syntax expression))))))

(define (transform-procedure-clause clause k)
  (define formals (clause-formals clause))
  (let ((c (make-location #f)))
    ((continuation-procedure k)
     (make-clause (make-formals
		   (cons c (formals-fixed-arguments formals))
		   (formals-rest-argument formals)
		   (formals-syntax formals))
		  (transform-body (clause-body clause) (make-reference c #f))
		  (clause-syntax clause)))))

(define (transform-sequence expression k)
  (make-sequence
   (transform-body (sequence-expressions expression) k)
   (expression-syntax expression)))

(define (transform-conditional expression k)
  (let ((c (make-location #f)))
    (make-procedure-call
     (generate-procedure
      (list c)
      #f
      (list
       (transform (conditional-test expression)
		  (lambda (a)
		    (make-conditional a				 
				      (transform (conditional-consequent expression)
						 (make-reference c #f))
				      (transform (conditional-alternate expression)
						 (make-reference c #f))
				      (expression-syntax expression))))))
     (list (continuation-expression k))
     #f)))

(define (transform-letrec-expression expression k)
  (do-transform* transform-letrec-binding
		 (letrec-expression-bindings expression)
		 (lambda (b*)
		   (make-letrec-expression
		    b*
		    (transform-body (letrec-expression-body expression) k)
		    (expression-syntax expression)))))

(define (transform-letrec-binding binding k)
  (transform-procedure (binding-expression binding)
		       (lambda (t)
			 ((continuation-procedure k)
			  (make-binding (binding-formals binding)
					t
					(binding-syntax binding))))))
  
(define (transform-let-values-expression expression k)
  (define binding (let-values-expression-binding expression))
  (transform (binding-expression binding)
	     (%generate-procedure
	      (make-clause
	       (binding-formals binding)
	       (transform-body (let-values-expression-body expression) k)
	       (expression-syntax expression)))))

(define (transform-body body k)
  (let-values
      ((body
	(let loop ((body body))
	  (let ((e (car body)) (e* (cdr body)))
	    (transform e
		       (if (null? e*)
			   k
			   (lambda (a)
			     (let-values ((a* (loop e*)))
			       (if (atomic? a)
				   (apply values a*)
				   (apply values a a*))))))))))
    body))

(define (do-transform* transformer x* k)
  (let do-transform* ((x* x*) (k k))
    (if (null? x*)
	((continuation-procedure k) '())
	(transformer (car x*)
		     (lambda (a)
		       (do-transform* (cdr x*)
				      (lambda (a*)
					((continuation-procedure k)
					 (cons a a*)))))))))

(define (transform* e* k)
  (do-transform* transform e* k))
