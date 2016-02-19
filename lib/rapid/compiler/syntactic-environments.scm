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

(define current-bindings (make-parameter #f box))
(define (get-bindings) (unbox (current-bindings)))
(define (set-bindings! bindings) (set-box! (current-bindings) bindings))

(define current-references (make-parameter #f box))
(define (get-references) (unbox (current-references)))
(define (set-references! references) (set-box! (current-references) references))

(define (with-syntactic-environment syntactic-environment thunk)
  (parameterize
      ((current-bindings (syntactic-environment-bindings syntactic-environment))
       (current-references (make-set (make-eq-comparator))))
    (thunk)))

(define (with-scope thunk)
  (let-values
      (((references result*)
	(parameterize
	    ((current-bindings (get-bindings))
	     (current-references (get-references)))
	  (let-values ((result* (thunk)))
	    (values (get-references) result*)))))
    (set-references! references)
    (apply values result*)))

(define (with-isolated-references thunk)
  (parameterize ((current-references (get-references)))
		(thunk)))

(define-record-type <syntactic-environment>
  (%make-syntactic-environment bindings)
  syntactic-environment?
  (bindings syntactic-environment-bindings))

(define-record-type <binding>
  (make-binding syntax denotation)
  syntactic-binding?
  (syntax binding-syntax)
  (denotation binding-denotation))

(define (binding-identifier binding)
  (syntax-datum (binding-syntax binding)))

(define (get-syntactic-environment)
  (%make-syntactic-environment (get-bindings)))

(define (make-syntactic-environment)
  (%make-syntactic-environment (make-map (make-eq-comparator))))

(define (binding-reference! binding)
  (set-references! (set-adjoin (get-references) binding)))

(define (binding-referenced? binding)
  (set-contains? (get-references) binding))

(define (%lookup-binding identifier)
  (map-ref/default (get-bindings) identifier #f))

(define (identifier-referenced? identifier)
  (let ((binding (%lookup-binding identifier)))
    (and binding (binding-referenced? binding))))

(define (lookup-binding! identifier)
  (cond
   ((%lookup-binding identifier)
    => (lambda (binding)
	 (binding-reference! binding)
	 binding))
   (else #f)))

(define (insert-binding! identifier-syntax denotation)
  (define identifier (syntax-datum identifier-syntax))
  (when (identifier-referenced? identifier)
    (compile-error "identifier has already been referenced" identifier-syntax))
  (let ((binding (make-binding identifier-syntax denotation)))
    (set-bindings! (map-set (get-bindings) identifier binding))
    (binding-reference! binding)))

(define (lookup-syntax! identifier)
  (cond
   ((lookup-binding! identifier) => binding-syntax)
   (else #f)))

(define (lookup-denotation! identifier)
  (cond
   ((lookup-binding! identifier) => binding-denotation)
   (else #f)))

(define (%insert-binding! identifier-syntax denotation)
  (define identifier (syntax-datum identifier-syntax))
  (cond
   ((%lookup-binding identifier)
    => (lambda (binding)
	 (unless (eq? (binding-denotation binding) denotation)
	   (compile-note "initial binding was here"
			 (binding-syntax binding))
	   (compile-error "identifier rebound with different denotation"
			  identifier-syntax))))
   (else
    (insert-binding! identifier-syntax denotation))))

(define insert-binding-from!
  (case-lambda
   ((identifier-syntax syntactic-environment)
    (insert-binding-from! identifier-syntax syntactic-environment identifier-syntax))
   ((identifier-syntax syntactic-environment new-identifier-syntax)
    (define identifier (syntax-datum identifier-syntax))
    (cond
     ((with-syntactic-environment
       syntactic-environment
       (lambda () (%lookup-binding identifier)))
      => (lambda (binding)
	   (%insert-binding! new-identifier-syntax (binding-denotation binding))))
     (else
      (compile-error (format "unbound identifier ‘~a’" identifier) identifier-syntax))))))
   
(define (delete-binding! identifier-syntax)
  (define identifier (syntax-datum identifier-syntax))
  (unless (%lookup-binding identifier)
	  (compile-error (format "unbound identifier ‘~a’" identifier) identifier-syntax))
  (set-bindings! (map-delete (get-bindings) identifier)))

(define derive-syntactic-environment
  (case-lambda
   ((syntactic-environment syntax)
    (derive-syntactic-environment syntactic-environment syntax (lambda (identifier) identifier)))
   ((syntactic-environment syntax rename)
    (with-syntactic-environment
     (make-syntactic-environment)
     (lambda ()
       (map-for-each
	(lambda (binding)
	  (%insert-binding! (derive-syntax (rename (binding-identifier binding)) syntax)
			    (binding-denotation binding)))
	(with-syntactic-environment syntactic-environment (lambda () (get-bindings))))
       (get-syntactic-environment))))))

(define (insert-bindings-from! syntactic-environment)
  (map-for-each
   (lambda (binding)
     (%insert-binding! (derive-syntax (binding-identifier binding)
				      (binding-syntax binding))
		       (binding-denotation binding)))
   (with-syntactic-environment syntactic-environment (lambda () (get-bindings)))))

(define-syntax syntactic-environment
  (syntax-rules ()
    ((syntactic-environment (identifier denotation) ...)
     (with-syntactic-environment
      (make-syntactic-environment)
      (lambda ()
	(insert-binding! (datum->syntax 'identifier) denotation)
	...
	(get-syntactic-environment))))))
