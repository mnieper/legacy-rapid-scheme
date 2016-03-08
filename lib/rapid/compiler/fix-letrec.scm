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

(define current-location-proxy-table (make-parameter #f))
(define current-complex! (make-parameter (lambda () #f)))
(define (set-location-proxy! location location-proxy)
  (table-set! (current-location-proxy-table) location location-proxy))
(define (complex!) ((current-complex!)))

(define (fix-letrec expression)
  (parameterize ((current-location-proxy-table
		  (make-table (make-eq-comparator))))
    (%fix-letrec expression)))

(define (make-location-proxy) (%make-location-proxy #f))
(define (%make-location-proxy free-reference-adder)
  (vector free-reference-adder))
(define (location-proxy-free-reference-adder location-proxy)
  (vector-ref location-proxy 0))
(define (location-proxy-set-free-reference-adder! location-proxy free-reference-adder)
  (vector-set! location-proxy 0 free-reference-adder))

(define (add-free-reference! location)
  (define proxy (table-ref/default (current-location-proxy-table) location #f))
  (when proxy
    ((location-proxy-free-reference-adder proxy) location)))

(define (%fix-letrec expression)
  (cond
   ((reference? expression)
    (add-free-reference! (reference-location expression))
    expression)
   ((procedure-call? expression)
    (complex!)
    (expression-map %fix-letrec expression))
   ((primitive-operation? expression)
    (complex!)
    (expression-map %fix-letrec expression))
   ((expression-procedure? expression)
    (parameterize ((current-complex! (lambda () #f)))
      (expression-map %fix-letrec expression)))
   ((assignment? expression)
    (complex!)
    (add-free-reference! (assignment-location expression))
    (expression-map %fix-letrec expression))
   ((letrec*-expression? expression)
    (fix-letrec*-expression expression))
   ((or (let-values-expression? expression)
	(letrec-expression? expression)
	(multiple-assignment? expression))
    (error "unhandled expression type" expression))
   (else
    (expression-map %fix-letrec expression))))

;;; XXX
;; XXX: Remove me after debugging
(define (log . args)
  (for-each
   (lambda (arg)
     (if (binding? arg)
	 (log-binding arg)
	 (display arg (current-error-port)))
     (display " " (current-error-port))
     #t)
   args)
  (newline (current-error-port))
  #t)


(define (log-binding binding)
  (define location
   (car (formals-fixed-arguments (binding-formals binding))))
  (if (location-syntax location)
      (display (syntax-datum (location-syntax location))
	       (current-error-port))
      (display location (current-error-port))))
  

;;;


(define (make-binding-datum complex? free-references transformed-init)
  (vector complex? free-references transformed-init (make-table (make-eq-comparator)) #f))
(define (binding-datum-complex? binding-datum)
  (vector-ref binding-datum 0))
(define (binding-datum-free-references binding-datum)
  (vector-ref binding-datum 1))
(define (binding-datum-transformed-init binding-datum)
  (vector-ref binding-datum 2))
(define (binding-datum-dependency-table binding-datum)
  (vector-ref binding-datum 3))
(define (binding-datum-referenced? binding-datum)
  (vector-ref binding-datum 4))
(define (binding-datum-reference! binding-datum)
  (vector-set! binding-datum 4 #t))


;; Problem: body may reference variable!!!
(define (fix-letrec*-expression expression)
  (define bindings (letrec*-expression-bindings expression))
  (define proxy (make-location-proxy))
  (define location-table (make-table (make-eq-comparator)))
  (define binding-datum-table (make-table (make-eq-comparator)))
  (define (binding-datum binding) (table-ref binding-datum-table binding))
  (define (binding-transformed-init binding)
    (binding-datum-transformed-init (binding-datum binding)))
  (define (binding-complex? binding)
    (binding-datum-complex? (binding-datum binding)))
  (define (add-dependency! binding1 binding2)
    (table-set! (binding-datum-dependency-table (binding-datum binding1)) binding2 #t))
  (define (binding-dependency-list binding)
    (table-keys (binding-datum-dependency-table (binding-datum binding))))
  (define (binding-depends-on? binding1 binding2)
    (table-ref/default (binding-datum-dependency-table (binding-datum binding1)) binding2 #f))
  (define (binding-free-references binding)
    (binding-datum-free-references (binding-datum binding)))
  (define (binding-referenced? binding)
    (binding-datum-referenced? (binding-datum binding)))
  (define (binding-reference! binding)
    (binding-datum-reference! (binding-datum binding)))
  (define (insert-location! location binding)
    (table-set! location-table location binding))
  (define (lookup-location location)
    (table-ref location-table location))
  (define (lambda-binding? binding)
    (and
     (expression-procedure? (binding-transformed-init binding))
     (or (formals-location (binding-formals binding))
	 (compile-error "not enough values" (expression-syntax binding)))
     #t))
  (define (make-bindings binding thunk)
    (let loop ((locations (formals-locations (binding-formals binding))))
      (if (null? locations)
	  (thunk)
	  (list
	   (make-let-values-expression
	    (make-binding
	     (make-formals (list (car locations)) #f)
	     (make-undefined #f)
	     #f)
	    (loop (cdr locations))
	    #f)))))
  (define (make-assignments binding thunk)
    (cons
     (make-multiple-assignment
      (binding-formals binding)
      (binding-transformed-init binding)
      (binding-syntax binding))
     (thunk)))
  ;; Record locations for each binding
  (for-each
   (lambda (binding)
     (for-each
      (lambda (location)
	(insert-location! location binding)
	(set-location-proxy! location proxy))
      (formals-locations (binding-formals binding))))
   bindings)
  ;; Transforms inits and record free references and complexity
  (for-each
   (lambda (binding)
     (define complex #f)
     (define free-references (make-table (make-eq-comparator)))
     (define (add-free-reference! location)
       (table-set! free-references location #t))
     (define (complex!)
       (set! complex #t))
     ;; FIXME: Use parameterize
     (location-proxy-set-free-reference-adder! proxy add-free-reference!)
     (let ((transformed-init
	    (parameterize ((current-complex! complex!))
	      (%fix-letrec (binding-expression binding)))))
       (table-set!
	binding-datum-table
	binding
	(make-binding-datum complex? (table-keys free-references) transformed-init))))
   bindings)
  ;; Record dependencies
  (let ((complex-binding #f))
    (for-each
     (lambda (binding)
       (for-each
	(lambda (location)
	  (define location-binding (lookup-location location))
	  (add-dependency! location-binding binding)
	  (binding-reference! location-binding))
	(binding-free-references binding))
       (when (binding-complex? binding)
	 (when complex-binding
	   (add-dependency! complex-binding binding))
	 (set! complex-binding binding)))
     bindings))
  (let* ((sccs
	  ;; Build dependency graph
	  (graph-scc
	   (map
	    (lambda (binding)
	      (cons binding (binding-dependency-list binding)))
	    bindings)
	   (make-eq-comparator))))
    ;; Construct transformed expression
    (make-sequence
     (let loop ((sccs sccs))
       (define (rest) (loop (cdr sccs)))
       (if
	(null? sccs)
	;; Transform the body
	(map-in-order %fix-letrec (letrec*-expression-body expression))
	;; Transform next scc of bindings
	(let ((scc (car sccs)))
	  (if (= (length scc) 1)
	      ;; Single bindings
	      (let ((binding (car scc)))
		(cond
		 ;; Single procedure binding
		 ((lambda-binding? binding)
		  (if (binding-referenced? binding)
		      (list
		       (make-letrec-expression		    
			(list
			 (make-binding
			  (binding-formals binding)
			  (binding-transformed-init binding)
			  (binding-syntax binding)))
			(rest)
			#f))
		      (rest)))
		 ;; Single simple binding
		 ((not (binding-depends-on? binding binding))
		  (cond
		   ((binding-referenced? binding)
		    ;; somehow, value_174 is not being referenced
		    (list
		     (make-let-values-expression
		      (make-binding
		       (binding-formals binding)
		       (binding-transformed-init binding)
		       (binding-syntax binding))
		      (rest)
		      #f)))
		   ((binding-complex? binding)
		    (cons (binding-transformed-init binding)
			  (rest)))
		   (else
		    (rest))))
		 ;; Single complex binding
		 (else
		  (make-bindings
		   binding
		   (lambda ()
		     (make-assignments binding rest))))))
	      ;; Multiple bindings
	      (let loop ((binding* scc))
		(if (null? binding*)
		    (list
		     (make-letrec-expression
		      (let loop ((binding* scc))
			(if (null? binding*)
			    '()
			    (let ((binding (car binding*)))
			      (if (lambda-binding? binding)
				  (cons
				   (make-binding
				    (binding-formals binding)
				    (binding-transformed-init binding)
				    (binding-syntax binding))
				   (loop (cdr binding*)))
				  (loop (cdr binding*))))))
		      (let loop ((binding* scc))
			(if (null? binding*)
			    (rest)
			    (cond
			     ((lambda-binding? (car binding*))
			      (loop (cdr binding*)))
			     ((binding-referenced? (car binding*))
			      (make-assignments (car binding*)
						(lambda () (loop (cdr binding*)))))
			     (else
			      (cons
			       (binding-transformed-init (car binding*))
			       (loop (cdr binding*)))))))
		      #f))
		    (let ((binding (car binding*)))
		      (if (or (lambda-binding? binding) (not (binding-referenced? binding)))
			  (loop (cdr binding*))
			  (make-bindings (car binding*)
					 (lambda () (loop (cdr binding*))))))))))))
     (expression-syntax expression))))
