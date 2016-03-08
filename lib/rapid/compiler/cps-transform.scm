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

(define (cps-transform expression)
  (parameterize ((current-continuation (lambda (res) res))) ;; FIXME
    (%cps-transform expression)))

(define current-continuation (make-parameter #f))

(define (call-continuation . expression*)
  (define k (current-continuation))
  (if (proc? k)
      (apply k expression*)
      (make-procedure-call k expressions* #f)))

(define (call-with-continuation proc)
  (let-values ((a* (call-with-current-continuation proc)))
    (apply call-continuation a*)))

(define (syntactic-continuation)
  (define k (current-continuation))
  (if (proc? k)
      (let ((c (make-location #f)))
	(let-values ((body (k (make-reference c #f))))
	  (make-continuation-procedure (list c) body)))
      k))

(define (make-continuation-procedure arguments body)
  (make-procedure
   (list
    (make-clause (make-formals arguments #f) body #f)
    ;; TODO: Fall-through for if continuation is called with the wrong number of values
    )
   #f))

(define (atomic? expression)
  (or (reference? expression)
      (literal? expression)
      (undefined? expression)))
  
(define (%cps-transform expression)
  (cond
   ((atomic? expression)
    (call-continuation expression))
   ((procedure-call? expression)
    (cps-transform-procedure-call expression))
   ((primitive-operation? expression)
    (if (eq? (operator-identifier (primitive-operation-operator expression))
	     'call-with-current-continuation)
	(cps-transform-call/cc expression)
	(cps-transform-primitive-operation expression)))
   ((expression-procedure? expression)
    (cps-transform-procedure expression))
   ((assignment? expression)
    (cps-transform-assignment expression))
   ((multiple-assignment? expression)
    (cps-transform-multiple-assignment expression))

   ;; ...
   ;; TODO
   ;;
   ;; letrec, let-values, sequence, conditional
   ;;
   ;; ...
   
   ;; The general recursive binding construct should have already been
   ;; eliminated by a previous fix-letrec pass.
   ((letrec*-expression? expression)
    (error "unhandled expression type" expression))
   (else
    (expression-map %cps-transform expression))))

(define (cps-transform-procedure-call expression)
  (define k (syntactic-continuation))
  (parameterize
      ((current-continuation
	(lambda (t*)
	  (make-procedure-call (car t*) (cons k (cdr t*)) (expression-syntax syntax)))))   
    (cps-transform-term* (cons (procedure-call-operator expression)
			       (procedure-call-operands expression)))))

(define (cps-transform-primitive-operation expression)
  (call-with-continuation
   (lambda (k)
     (parameterize
	 ((current-continuation
	   (lambda (t*)
	     (k (make-primitive-operation (primitive-operation-operator expression)
					  t*
					  (expression-syntax expression))))))
       (cps-transform-term* (primitive-operation-operands expression))))))

(define (cps-transform-assignment expression)
  (call-with-continuation
   (lambda (k)
     (parameterize
	 ((current-continuation
	   (lambda (t)
	     (k (make-assignment (assignment-location expression)
				 t
				 (expression-syntax expression))))))
       (%cps-transform (assignment-expression expression))))))

(define (cps-transform-multiple-assignment expression)
  (define k (syntactic-continuation))
  (define formals (multiple-assignment-formals expression))
  (define new-formals
    (make-formals
     (map (lambda (argument) (make-location #f)) (formals-fixed-arguments formals))
     (and (formals-rest-argument formals)
	  (make-location #f))
     #f))
  (parameterize
      ((current-continuation
	(make-procedure new-formals
			(append
			 (map
			  (lambda (argument new-argument)
			    (make-assignment argument (make-reference new-argument #f) #f))
			  (formals-fixed-arguments formals)
			  (formals-fixed-arguments new-formals))
			 (if (formals-rest-argument formal)
			     (list
			      (make-assignment (formals-rest-argument formals)
					       (make-reference
						(formals-rest-argument new-formals)
						#f)
					       #f))
			     '())
			 (list
			  (make-procedure-call k (list (make-undefined #f)))))
			#f)))
    (%cps-transform (multiple-assignment-expression expression))))

(define (cps-transform-call/cc expression)
  (define operand (car (primitive-operation-operands expression)))
  (call-continuation
   (let* ((c (make-location #f))
	  (t (parameterize
		 ((current-continuation
		   (lambda (a)
		     (make-procedure-call a
					  (list (make-reference c) (make-reference c))
					  (expression-syntax expression)))))
	       (%cps-transform expression))))
     (make-continuation-procedure (list c) (list t)))))

(define (cps-transform-procedure expression)
  (call-with-continuation
   (lambda (k)
     (parameterize
	 ((current-continuation
	   (lambda (c*)
	     (k (make-procedure c* (expression-syntax expression))))))
       (cps-transform* cps-transform-procedure-clause (procedure-clauses expression))))))

(define (cps-transform-procedure-clause clause)
  (define formals (clause-formals clause))
  (call-continuation
   (let ((c (make-location #f)))
     (let ((a*
	    (parameterize
		((current-continuation (make-reference c)))
	      (transform-body (clause-body clause)))))
       (make-clause (make-formals
		     (cons c (formals-fixed-arguments formals))
		     (formals-rest-argument formals)
		     (formals-syntax formals))
		    a*
		    (clause-syntax clause))))))

(define (cps-transform-body body)
  (define k (syntactic-continuation))
  (let-values
      ((body
	(let loop ((body body))
	  (let ((e (car body)) (e* (cdr body)))
	    (parameterize
		((current-continuation
		  (if (null? e*)
		      k
		      (lambda (a)
			(let-values ((a* (loop e*)))
			  (if (atomic? a)
			      (apply values a*)
			      (apply values a a*)))))))
	      (%cps-transform e))))))
    body))

;; TODO: Rename into for-each/map/something similar
(define (cps-transform* transformer x*)
  (let cps-transform* ((x* x*))
    (call-with-continuation
     (lambda (k)
       (if (null? e*)
	   (k '())
	   (parameterize
	       ((current-continuation
		 (lambda (a)
		   (parameterize
		       ((current-continuation
			 (lambda (a*)
			   (k (cons a a*)))))		  
		     (cps-transform* (cdr e*))))))				       
	     (transformer (car e*))))))))
  

(define (cps-transform-term* e*)
  (cps-transform* %cps-transform e*))
