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

(define-record-type <syntactic-environment>
  (%make-syntactic-environment bindings)
  syntactic-environment?
  (bindings syntactic-environment-bindings))

;; FIXME TODO
(define-syntax syntactic-environment
  (syntax-rules ()
    ((syntactic-environment (identifier denotation) ...)
     (...))))

     
(define (make-binding syntax denotation timestamp)
  (vector syntax denotation timestamp))
(define (binding-syntax binding) (vector-ref binding 0))
(define (binding-denotation binding) (vector-ref binding 1))
(define (binding-timestamp binding) (vector-ref binding 2))
(define (binding-set-timestamp! binding timestamp) (vector-set! binding 2 timestamp))

(define timestamp 0)
(define current-timestamp (make-parameter 0))
(define timestamps (make-parameter (make-interval-set)))
(define (identifier-referenced? identifier syntactic-environment)
  (cond
   ((map-lookup (syntactic-environment-bindings syntactic-environment) identifier)
    => (lambda (binding)
	 (interval-set-contains? (timestamps) (binding-timestamp binding))))
   (else #f)))

(define (capture-references thunk proc)
  (define start (+ timestamp 1))
  (set! timestamp start)
  (call-with-values
      (lambda ()
	(parameterize ((current-timestamp timestamp)
		       (timestamps (interval-set-insert (timestamps) start +inf.0))) (thunk)))
    (lambda values
      (parameterize ((timestamps (interval-set-insert (timestamps) start timestamp)))
		    (apply proc values)))))

(define (update-timestamp! binding)
  (binding-set-timestamp! binding (max (binding-timestamp binding) (current-timestamp))))

(define (make-syntactic-environment)
  (%make-syntactic-environment (make-map (lambda (identifier) #f) eq?)))

(define (lookup-denotation identifier syntactic-environment)
  (cond
   ((map-lookup (syntactic-environment-bindings syntactic-environment) identifier)
    => (lambda (binding)
	 (update-timestamp! binding)
	 (binding-denotation binding)))
   (else #f)))

(define (lookup-syntax identifier syntactic-environment)
  (cond
   ((map-lookup (syntactic-environment-bindings syntactic-environment) identifier)
    => binding-syntax)
   (else #f)))

(define (insert-binding identifier-syntax denotation syntactic-environment)
  (%make-syntactic-environment
   (map-insert (syntactic-environment-bindings syntactic-environment)
	       (syntax-datum identifier-syntax)
	       (make-binding identifier-syntax denotation -1))))

(define (%insert-binding identifier-syntax denotation syntactic-environment)
  (define identifier (syntax-datum identifier-syntax))
  (cond
   ((lookup-denotation identifier syntactic-environment)
    => (lambda (previous-denotation)
	 (if (not (eq? denotation previous-denotation))
	     (compile-error (identifier-syntax
			     ("identifier ‘~a’ rebound with different denotation" identifier))
			    ((lookup-syntax identifier syntactic-environment)
			     ("initial binding was here")))
	     syntactic-environment)))
   (else
    (insert-binding identifier-syntax denotation syntactic-environment))))

(define insert-binding-from
  (case-lambda
   ((identifier-syntax syntactic-environment1 syntactic-environment)
    (insert-binding-from identifier-syntax
			 syntactic-environment1
			 syntactic-environment
			 identifier-syntax))
   ((identifier-syntax syntactic-environment1 syntactic-environment new-identifier-syntax)
    (cond
     ((map-lookup (syntactic-environment-bindings syntactic-environment1)
		  (syntax-datum identifier-syntax))
      => (lambda (binding)
	   (%insert-binding new-identifier-syntax
			    (binding-denotation binding)
			    syntactic-environment)))
     (else
      (compile-error (identifier-syntax
		      ("identifier ‘~a’ not found" (syntax-datum identifier)))))))))

(define (delete-binding identifier syntactic-environment)
  (%make-syntactic-environment
   (map-delete (syntactic-environment-bindings syntactic-environment)
	       identifier)))

(define (insert-bindings-from syntactic-environment1 syntactic-environment)
  (define bindings (syntactic-environment-bindings syntactic-environment))
  (let loop ((bindings1-alist
	      (map->alist (syntactic-environment-bindings syntactic-environment1)))
	     (syntactic-environment syntactic-environment))
    (if (null? bindings1-alist)
	syntactic-environment
	(loop (cdr bindings1-alist)
	      (%insert-binding (derive-syntax (caar bindings1-alist)
					      (binding-syntax (cdar bindings1-alist)))
			       (binding-denotation (cdar bindings1-alist))
			       syntactic-environment)))))

(define derive-syntactic-environment
  (case-lambda
   ((syntactic-environment syntax)
    (derive-syntactic-environment syntactic-environment syntax (lambda (identifier) identifier)))
   ((syntactic-environment syntax rename)
    (let loop ((bindings-alist (map->alist (syntactic-environment-bindings syntactic-environment)))
	       (new-environment (make-syntactic-environment)))
      (if (null? bindings-alist)
	  new-environment
	  (loop (cdr bindings-alist)
		(%insert-binding (derive-syntax (rename (caar bindings-alist)) syntax)
				 (binding-denotation (cdar bindings-alist))
				 new-environment)))))))
