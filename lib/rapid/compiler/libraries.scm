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

;;; TODO
;; Refactor bindings and library-table to environments
;; Do a similar thing as we did with syntactic-environments
;; Split this (possibly) into import-sets and libraries

;;; FIXME
;; Contexts should not be nested due to import specs.

(define (expand-import-sets import-sets)
  (define bindings (reverse (environment-bindings primitive-environment)))
  (define library-table   ;; XXX: when we use environment, we can use primitive! 
    (let ((table (make-table (make-equal-comparator))))
      (table-set! table
		  '(rapid primitive)
		  (environment-syntactic-environment primitive-environment))
      table))
  (define (lookup-synthetic-environment library-name)
    (table-ref/default library-table library-name #f))
  (define (insert-library! library-name)
    (table-set! library-table library-name #f))    
  (define (library-loading? library-name)
    (not (table-ref/default library-table library-name #t)))
  (define (create-syntactic-environment import-sets)
    (with-syntactic-environment
     (make-syntactic-environment)
     (lambda ()
       (for-each
	(lambda (import-set)
	  (insert-bindings-from! (read-import-set import-set)))
	import-sets)
       (get-syntactic-environment))))
  ;; Returns a syntactic environment
  (define (read-import-set import-set)
    (define form (syntax-datum import-set))
    (unless (list? form)
      (compile-error "bad import set" import-set))
    (cond
     ;; Import set modifier
     ((and (> (length form) 1) (list? (syntax-datum (cadr form))))
      (let ((syntactic-environment (read-import-set (cadr form))))
	(case (syntax-datum (car form))
	  ;; Only import set
	  ((only)
	   (with-syntactic-environment
	    (make-syntactic-environment)
	    (lambda ()
	      (for-each
	       (lambda (identifier-syntax)
		 (assert-identifier! identifier-syntax)
		 (insert-binding-from! identifier-syntax syntactic-environment))
	       (cddr form)))
	    (get-syntactic-environment)))
	  ;; Except import set
	  ((except)
	   (with-syntactic-environment
	    (derive-syntactic-environment syntactic-environment import-set)
	    (lambda ()
	      (for-each
	       (lambda (identifier-syntax)
		 (assert-identifier! identifier-syntax)
		 (delete-binding! identifier-syntax))
	       (cddr form))
	      (get-syntactic-environment))))
	  ;; Prefix import set
	  ((prefix)
	   (unless (and (= (length form) 3)
			(symbol? (syntax-datum (caddr form))))
	     (compile-error "bad import set" import-set))
	   (derive-syntactic-environment syntactic-environment
					 import-set
					 (lambda (identifier)
					   (symbol-append
					    (syntax-datum (caddr form))
					    identifier))))
	  ;; Rename import set
	  ((rename)
	   (let ((table (make-table (make-eq-comparator))))
	     (for-each
	      (lambda (rename)
		(define form (syntax-datum rename))
		(unless (and (list? form)
			     (= (length form) 2)
			     (symbol? (syntax-datum (car form)))
			     (symbol? (syntax-datum (cadr form))))
		  (compile-error "bad rename" rename))
		(table-set! table (syntax-datum (car form)) (syntax-datum (cdr form))))
	      (cddr form))
	     (derive-syntactic-environment syntactic-environment
					   import-set
					   (lambda (identifier)
					     (or (table-ref/default table identifier #f)
						 identifier)))))
	  (else (compile-error "invalid import set" import-set)))))
     ;; Simple import
     (else
      (assert-library-name! import-set)
      (derive-syntactic-environment (read-library import-set) import-set))))
  ;; Returns the syntactic environment of a library
  ;; Adds entries to library table if library cannot be found
  ;; Adds bindings
  (define (read-library library-name-syntax)
    (define library-name (syntax->datum library-name-syntax))
    (cond
     ((lookup-synthetic-environment library-name))
     (else
      (when (library-loading? library-name)
	(compile-error "library references itself while loading" library-name-syntax))
      (insert-library! library-name)
      (let ((library-definition (read-library-definition library-name-syntax)))
	(define form (syntax-datum library-definition))
	(define-values (import-sets export-specs body)
	  (let loop ((declarations (cddr form))
		     (import-sets '())
		     (export-specs '())
		     (body '()))
	    (if (null? declarations)
		(values (reverse import-sets) (reverse export-specs) (reverse body))
		(let* ((declaration (car declarations))
		       (declarations (cdr declarations))
		       (form (syntax-datum declaration)))
		  (unless (and (not (null? form)) (list? form))
		    (compile-error "bad library declaration" declaration))
		  (case (syntax-datum (car form))
		    ((export)
		     (loop declarations
			   import-sets
			   (append (reverse (cdr form)) export-specs)
			   body))
		    ((import)
		     (loop declarations
			   (append (reverse (cdr form)) import-sets)
			   export-specs
			   body))
		    ((begin)
		     (loop declarations
			   import-sets
			   export-specs
			   (append (reverse (cdr form)) body)))
		    ((include)
		     (loop declarations
			   import-sets
			   export-specs
			   (generator-fold cons body (read-file* (cdr form) #f))))
		    ((include-ci)
		     (loop declarations
			   import-sets
			   export-specs
			   (generator-fold cons body (read-file* (cdr form) #t))))
		    ((include-library-declarations)
		     (loop (generator-fold cons declarations (read-file (cdr form) #f))
			   import-sets
			   export-specs
			   body))			   
		    ((cond-expand)
		     (let loop-clauses ((clauses (cdr form)))
		       (if (null? clauses)
			   (loop declarations import-sets export-specs body)
			   (let ((clause (car clauses)))
			     (define form (syntax-datum clause))
			     (unless (and (list? form) (>= (length form) 1))
			       (compile-error "bad cond-expand clause" clause))
			     (cond
			      ((eq? (car form) 'else)
			       (unless (null? (cdr clauses))
				 (compile-error "else clause not last" declaration))
			       (loop (append (cdr form) declarations)
				     import-sets
				     export-specs
				     body))
			      ((feature? (car form))
			       (loop (append (cdr form) declarations)
				     import-sets
				     export-specs
				     body))
			      (else
			       (loop-clauses (cdr clauses))))))))
		    (else
		     (compile-error "invalid library declaration" declaration)))))))
	(with-syntactic-environment
	 (create-syntactic-environment import-sets)
	 (lambda ()
	   (set! bindings (append (reverse (expand-top-level body)) bindings))
	   (let ((syntactic-environment (get-syntactic-environment)))
	     (with-syntactic-environment
	      (make-syntactic-environment)
	      (lambda ()
		(for-each
		 (lambda (export-spec)
		   (define form (syntax-datum export-spec))
		   (cond
		    ((list? form)
		     (unless (= (length form) 2)
		       (compile-error "bad export spec" export-spec))
		     (assert-identifier! (car form))
		     (assert-identifier! (cadr form))
		     (insert-binding-from! (car form) syntactic-environment (cadr form)))
		    (else
		     (assert-identifier! export-spec)
		     (insert-binding-from! export-spec syntactic-environment))))
		 export-specs)
		(get-syntactic-environment))))))))))
  (define syntactic-environment (create-syntactic-environment import-sets))
  (make-environment (reverse bindings) syntactic-environment))

(define (feature? feature-requirement-syntax)
  (define form (syntax-datum feature-requirement-syntax))
  (cond
   ((symbol? form)
    (assq form rapid-features))
   ((and (not (null? form)) (list? form))
    (case (syntax-datum (car form))
      ((library)
       (unless (= (length form) 2)
	 (compile-error "bad library feature requirement" feature-requirement-syntax))
       (guard (condition
	       ((compile-error-object? condition) #f))
	      (read-library-definition (cadr form))
	      #t))
      ((and)
       (let loop ((feature-requirement-syntax* (cdr form)))
	 (if (null? feature-requirement-syntax*)
	     #t
	     (let* ((r1 (feature? (car feature-requirement-syntax*)))
		    (r2 (loop (cdr feature-requirement-syntax*))))
	       (and r1 r2)))))
      ((or)
       (let loop ((feature-requirement-syntax* (cdr form)))
	 (if (null? feature-requirement-syntax*)
	     #f
	     (let* ((r1 (feature? (car feature-requirement-syntax*)))
		    (r2 (loop (cdr feature-requirement-syntax*))))
	       (or r1 r2)))))
      ((not)
       (unless (= (length form) 2)
	 (compile-error "bad not feature requirement" feature-requirement-syntax))
       (not (feature? (cadr form))))
      (else
       (compile-error "invalid feature requirement" feature-requirement-syntax))))
   (else
    (compile-error "bad feature requirement" feature-requirement-syntax))))

(define (assert-identifier! identifier-syntax)
  (unless (symbol? (syntax-datum identifier-syntax))
    (compile-error (format "bad identifier ‘~a’" (syntax-datum identifier-syntax))
		   identifier-syntax)))

(define (assert-library-name! library-name-syntax)
  (define form (syntax-datum library-name-syntax))
  (define (library-name?)
    (and (list? form)
	 (let loop ((form form))
	   (or (null? form)
	       (let ((datum (syntax-datum (car form))))
		 (and (or (and (exact-integer? datum) (>= datum 0))
			  (symbol? datum))
		      (loop (cdr form))))))))
  (unless (library-name?)
    (compile-error "bad library name" library-name-syntax)))

(define (read-library-definition library-name-syntax)
  (define library-name (syntax->datum library-name-syntax))
  (define (locate-library)
    ;; TODO: error handling
    ;; TODO: search several directories
    (let loop ((filename "share") (library-name library-name))
      (if (null? library-name)
	  (string-append filename ".sld")
	  (loop (path-join filename (symbol->string (car library-name)))
		(cdr library-name)))))
  (define source (locate-library))
  (call-with-input-file source
    (lambda (port)  
      (define source-port (make-source-port port source #f))   ;; TODO: use read-file ?
      (let loop ()
	(define syntax (read-syntax source-port library-name-syntax))
	(when (eof-object? syntax)
	  (compile-error (format "library definition of ‘~a’ not found in file ‘~a’"
				 library-name-syntax 
				 source)
			 library-name-syntax))
	(let ((form (syntax-datum syntax)))
	  (cond
	   ((and (list? form)
		 (>= (length form) 2)
		 (eq? (syntax-datum (car form)) 'define-library))
	    (assert-library-name! (cadr form))
	    (if (equal? (syntax->datum (cadr form)) library-name)
		syntax
		(loop)))
	   (else
	    (loop))))))))

(define (read-file* string-syntax* ci?)
  (apply gappend (map-in-order
		  (lambda (string-syntax)
		    (%read-file string-syntax ci?))
		  string-syntax*)))

(define (%read-file string-syntax ci?)
  (define filename (syntax-datum string-syntax))
  (unless (string? filename)
    (compile-error "bad string literal" string-syntax))
  (read-file filename ci? string-syntax))

(define (symbol-append symbol1 symbol2)
  (string->symbol
   (string-append (symbol->string symbol1) (symbol->string symbol2))))
