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
(define current-free-references-table (make-parameter #f))
(define (set-location-proxy! location location-proxy)
  (table-set! (current-location-proxy-table) location location-proxy))

(define (fix-letrec expression)
  (parameterize ((current-location-proxy-table
		  (make-table (make-eq-comparator)))
		 (current-free-references-table
		  (make-table (make-eq-comparator))))
    ;; add current-complex!
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

(define (find-free-references! expression)
  (let loop ((expression expression))
    (cond
     ((letrec*-expression? expression)
     ((reference? expression)
      (add-free-reference! (reference-location expression)))
     (else
      (expression-for-each loop expression)))))

(define (%fix-letrec expression)    
  (cond
   ((reference? expression? expression)
    (add-free-reference! (reference-location expression)))
   ((procedure-call? expression)
    (complex!)
    (expression-map %fix-letrec expression))
   ((primitive-operation? expression)
    (complex!)
    (expression-map %fix-letrec expression))
   ((expression-procedure?
     (parameterize ((current-complex! (lambda () #f)))
       (expression-map %fix-letrec expression))))
   ((assignment?
     (complex!)
     (expression-map %fix-letrec expression)))   
   ((letrec*-expression? expression)
    (fix-letrec*-expression expression))
   (else
    (expression-map %fix-letrec expression))))

(define (make-binding-datum binding complex? free-references transformed-init dependent-datums)
  (vector binding complex? free-references transformed-init (make-table (make-eq-comparator))))

(define (fix-letrec*-expression expression)
  (define bindings (letrec*-expression-bindings expression))
  (define proxy (make-location-proxy))
  (define binding-datum-table (make-table (make-eq-comparator)))
  (define location-table (make-table-eq-comparator))
  (define (insert-location! location binding)
    (table-set! location-table location binding))
  (define (lookup-location location)
    (table-ref location-table location))
  (for-each
   (lambda (binding)
     (for-each
      (lambda (location)
	(insert-location! location binding)
	(set-location-proxy! location proxy))
      (formals-locations (binding-formals binding))))
   (letrec*-expression-bindings expression))
  (for-each
   (lambda (binding)
     (define complex #f)
     (define free-references (make-table (make-eq-comparator)))
     (define (add-free-reference! location)
       (table-set! free-references location #t))
     (define (complex!)
       (set! complex #t))
     (location-proxy-set-free-reference-adder! proxy add-free-reference!)
     (let ((transformed-init
	    (parameterize ((current-complex! complex!))
	      (%fix-letrec (binding-expression binding)))))
       (table-set!
	binding-datum-table
	binding
	(make-binding-datum binding complex? free-references transformed-init))))
   (letrec*-expression-bindings expression))
  (let ((complex-binding #f))
    (for-each
     (lambda (binding)
       (for-each
	(lambda (location)
	  (define location-binding (lookup-location location))
	  (add-dependency! location-binding binding))
	(binding-free-references binding))
       (when (binding-complex? binding)
	 (when complex-binding
	   (add-dependency! complex-binding binding))
	 (set! complex-binding binding)))
     (letrec*-expression-bindings expression)))
  (let*
      ((sccs
	(graph-scc
	 (map
	  (lambda (binding)
	    (cons binding (binding-dependency-list binding)))
	  bindings)
	 (make-eq-comparator))))
    (let loop ((sccs sccs) (syntax (expression-syntax syntax)))
      (define (rest) (loop (cdr sccs) #f))
      (if
       (null? sccs)
       ;; Transform the body
       (map-in-order %fix-letrec (letrec*-expression-body expression))
       (let ((scc (car sccs)))
	 (if (= (length scc) 1)
	     (let ((binding (car scc)))
	       (cond
		;; Single procedure binding
		((expression-procedure? (binding-transformed-init binding))
		 (unless (formals-simple? (binding-formals binding))
		   (compile-error "not enough values" (expression-syntax binding)))
		 (make-letrec-expression
		  (list
		   (make-binding
		    (binding-formals binding)
		    (binding-transformed-init binding)
		    (expression-syntax binding)))
		  (rest)
		  syntax))
		;; Single simple binding
		((not (binding-depends-on? binding binding))
		 (make-let-expression
		  (make-binding
		   (binding-formals binding)
		   (binding-transformed-init binding)
		   (expression-syntax binding))
		  (rest)
		  syntax))
		;; Single complex binding
		(else 'handle-with-assignment)))
	     ('multiple-bindings)))
       sccs))))
