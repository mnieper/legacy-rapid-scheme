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

;; Description of the syntax rules macro compiler
;;
;; FIXME: The description does not describe the most recent verrsion of
;; the code.
;;
;; Each syntax rule is compiled separately. Each syntax rule consists of
;; a pattern and a template.
;; The pattern compiler takes as input the pattern. It returns two
;; values. The first value is a map that maps each free identifier in
;; the pattern to a pair consisting of an index and the ellipsis depth
;; of the identifier.
;; The second value is a matcher, a form that takes a datum
;; to match and that returns a vector of matched identifier bindings or
;; #f if there is no match. Identifiers with a non-trivial ellipsis
;; depth are match to lists.
;; The template compiler takes as input the template and the map of
;; identifiers that are free in the pattern. It returns a transcriber, a
;; form that takes a vector of matched identifier bindings and that
;; returns a syntax object.
;; In order to handle subpatterns, the pattern compiler calls a special
;; subpattern compiler, which basically has the same signature as the
;; pattern compiler itself, except that the pattern compiler strips the
;; syntax keyword from the input form.
;; In order to handle subtemplates, the template compiler calls a
;; special subtemplate compiler. The subtemplate compiler takes as input
;; the subtemplate and the map of identifiers that are free in the
;; pattern. It returns two values. The first value is a set of
;; free identifiers that are referenced. The second value is a form that
;; takes a vector of matched identifier bindings.

(define *transformer-environment*
  (environment '(scheme base)
	       '(rapid lists)))

(define-syntax eval-transformer
  (syntax-rules ()
    ((eval-transformer transformer identifier ...)
     ((eval `(lambda (identifier ...)
	       ,transformer)
	    *transformer-environment*)
      identifier ...))))

(define (make-er-macro-transformer transformer macro-environment)
  (lambda (syntax environment)
    (define renames (make-table (make-eq-comparator)))
    (define (rename identifier)
      (table-intern! renames
		     identifier
		     (lambda ()
		       (make-syntactic-closure macro-environment '() identifier))))
    (define (compare identifier1 identifier2)
      (identifier=? environment identifier1 environment identifier2))
    (transformer syntax rename compare)))

(define current-macro-environment (make-parameter #f))
(define current-ellipsis? (make-parameter #f))
(define current-literal? (make-parameter #f))
(define (ellipsis? identifier) ((current-ellipsis?) identifier))
(define (literal? identifier) ((current-literal?) identifier))

;; TODO: If ellipsis? is literal? => no ellipsis
(define (make-syntax-rules-transformer ellipsis?
				       literal?
				       syntax-rule-syntax*
				       transformer-syntax
				       macro-environment)
  (parameterize ((current-macro-environment macro-environment)
		 (current-ellipsis? ellipsis?)
		 (current-literal? literal?))
    (define syntax-rules-transformer
      (compile-syntax-rules-transformer syntax-rule-syntax*))
    (define pattern-syntax-vector
      (list->vector
       (map
	(lambda (syntax-rule-syntax)
	  (car (syntax-datum syntax-rule-syntax)))
	syntax-rule-syntax*)))
    (define template-syntax-vector
      (list->vector
       (map
	(lambda (syntax-rule-syntax)
	  (cadr (syntax-datum syntax-rule-syntax)))
	syntax-rule-syntax*)))
    (define er-macro-transformer
      (eval-transformer syntax-rules-transformer
			compile-error compile-note transformer-syntax
			syntax-datum derive-syntax
			template-syntax-vector
			pattern-syntax-vector))
    (make-er-macro-transformer er-macro-transformer macro-environment)))

(define (compile-syntax-rules-transformer syntax-rule-syntax*)
  (define clauses
    (let loop ((syntax-rule-syntax* syntax-rule-syntax*) (i 0))
      (if (null? syntax-rule-syntax*)
	  '()
	  (let ((syntax-rule (syntax-datum (car syntax-rule-syntax*))))
	    (unless (and (list? syntax-rule) (= (length syntax-rule) 2))
	      (compile-error "bad syntax-rule" (car syntax-rule-syntax*)))
	    (let*-values
		(((pattern-syntax)
		  (car syntax-rule))
		 ((template-syntax)
		  (cadr syntax-rule))
		 ((identifiers matcher)
		  (compile-pattern pattern-syntax i))
		 ((transcriber)
		  (compile-template template-syntax identifiers i))  ;; identifiers => variable-map
		 ((clause)
		  `(,matcher => ,transcriber)))
	      (cons clause (loop (cdr syntax-rule-syntax*) (+ i 1))))))))
  `(lambda (syntax rename compare)
     (define form (syntax-datum syntax))
     (cond
      ,@clauses
      (else
       (compile-note "the macro definition is here" transformer-syntax)
       (compile-error "no expansion for macro use" syntax)))))

(define (make-pattern-variable index depth syntax) (vector index depth syntax))
(define (pattern-variable-index variable) (vector-ref variable 0))
(define (pattern-variable-depth variable) (vector-ref variable 1))
(define (pattern-variable-syntax variable) (vector-ref variable 2))

(define (compile-pattern pattern-syntax rule-index)
  (define pattern (syntax-datum pattern-syntax))
  (unless (and (list? pattern) (>= (length pattern) 1))
    (compile-error "invalid pattern" pattern-syntax))
  (let-values (((identifiers matcher)
		(compile-subpattern (derive-syntax (cdr pattern) pattern-syntax))))
    (values identifiers `(,matcher (derive-syntax (cdr form) syntax)
				   (vector-ref pattern-syntax-vector ,rule-index)))))

(define (make-pattern-variable-map) (make-map (make-eq-comparator)))

(define (compile-subpattern pattern-syntax)
  (define pattern (syntax-datum pattern-syntax))
  (cond
   ((identifier? pattern)
    (cond
     ((literal? pattern)
      (values
       (make-pattern-variable-map)
      `(lambda (syntax pattern-syntax)
	 (and (compare (syntax-datum syntax) (syntax-datum pattern)) #()))))
     ((eq? pattern '_)
      (values
       (make-pattern-variable-map)
       `(lambda (syntax pattern-syntax)
	  #())))
     (else
      (values
       (map-set (make-pattern-variable-map) pattern (make-pattern-variable 0 0 pattern-syntax))
       `(lambda (syntax pattern-syntax)
	  (vector (syntax-datum syntax)))))))
   ((vector? pattern)
    (let-values
	(((variables-map matcher)
	  (compile-list-pattern (derive-syntax (vector->list pattern) pattern-syntax))))
      (values
       variables-map
       `(lambda (syntax pattern-syntax)
	  (let ((form (syntax-datum syntax)))
	    (and
	     (vector? form)
	     (let ((list (vector->list form)))
	       (,matcher (derive-syntax list syntax) (derive-syntax list pattern-syntax)))))))))
   ((null? pattern)
    (values
     (make-pattern-variable-map)
     `(lambda (syntax pattern-syntax)
	(null? (syntax-datum syntax)))))
   ((pair? pattern)
    (compile-list-pattern pattern-syntax))
   ((constant? pattern)
    (values
     (make-pattern-variable-map)
     `(lambda (syntax pattern-syntax)
	(equal? (syntax-datum syntax) (syntax-datum pattern)))))
   (else
    (compile-error "invalid subpattern" pattern-syntax))))

(define (compile-list-pattern pattern-syntax)
  (define pattern (syntax-datum pattern-syntax))
  (define (make-compiled-matcher variables matcher index)
    (vector variables matcher index))
  (define (compiled-matcher-variables matcher)
    (vector-ref matcher 0))
  (define (compiled-matcher-matcher matcher)
    (vector-ref matcher 1))
  (define (compiled-matcher-index matcher)
    (vector-ref matcher 2))
  (define-values (pattern-syntax1* pattern-syntax2* pattern-syntax3* pattern-syntax4*)
    (analyze-pattern-list pattern))
  (define-values (compiled-matcher1* variables count)
    ;; TODO: refactor in extra procedure
    (let loop ((syntax* pattern-syntax1*)
	       (compiled-matcher* '())
	       (variables (make-pattern-variable-map))
	       (index 0))
      (if (null? syntax*)
	  (values (reverse compiled-matcher*) variables index)
	  (let ((syntax (car syntax*)))
	    (define-values (subvariables matcher)
	      (compile-subpattern syntax))
	    (apply
	     (lambda (variables count)
	       (loop (cdr syntax*)
		     (cons (make-compiled-matcher subvariables matcher index)
			   compiled-matcher*)
		     variables
		     count))	     
	     (map-fold
	      subvariables
	      (lambda (identifier variable variables+count)
		(apply
		 (lambda (identifier variable variables count)
		   (cond
		    ((map-ref/default variables identifier #f)
		     => (lambda (old-variable)
			  (compile-note "first appearance was here" (pattern-variable-syntax
								     old-variable))
			  (compile-error "pattern variable has already appeared once"
					 (pattern-variable-syntax variable)))))
		   (list (map-set variables
				  identifier
				  (make-pattern-variable count
							 (pattern-variable-depth variable)
							 (pattern-variable-syntax variable)))
			 (+ count 1)))
		 identifier variable variables+count))
	      (list variables index)))))))
  ;; TODO: Refactor the whole code
  (define matcher
    `(lambda (syntax pattern-syntax)
       (define form (syntax-datum syntax))
       (when (circular-list? form)
	 (compile-error "circular list in source" syntax))
       (let* ((rest* (take-right form 0))
	      (form (drop-right form 0)))
	 (and
	  (,(if (not (null? pattern-syntax2*)) '>= '=)
	   (length form)
	   ,(+ (length pattern-syntax1*) (length pattern-syntax3*)))
	  (let*
	      ((form3 (take-right form ,(length pattern-syntax3*)))
	       (form (drop-right form ,(length pattern-syntax3*)))
	       (form1 (take form ,(length pattern-syntax1*)))
	       (form2 (drop form ,(length pattern-syntax1*)))
	       (match (make-vector ,count)))
	    (and ,@
	     (let loop ((compiled-matcher* compiled-matcher1*) (i 0))
	       (if (null? compiled-matcher*)
		   '()
		   (let* ((compiled-matcher (car compiled-matcher*))
			  (variables (compiled-matcher-variables compiled-matcher))
			  (matcher (compiled-matcher-matcher compiled-matcher))
			  (index (compiled-matcher-index compiled-matcher))
			  (test
			   `(let ((match1 (,matcher (list-ref form1 ,i)    ;; pattern-syntax-vec!
						    (list-ref (syntax-datum pattern-syntax) ,i))))
			      (and
			       match1
			       (begin ,@
				 (map-fold
				  variables
				  (lambda (identifier variable setter*) `
				    (cons
				     (vector-set! match
						  ,(+ i (pattern-variable-index variable))
						  (vector-ref
						   match1
						   ,(pattern-variable-index variable)))
				     setter*))
				  '())
				 #t)))))
		     (cons test (loop (cdr compiled-matcher*) (+ i 1))))))
	     match))))))
  (values
   variables
   matcher))

(define (compile-template template-syntax variables rule-index)
  (define-values (slots transcriber)
    (compile-subtemplate template-syntax variables 0))
  `(lambda (match)
     (,transcriber
      (vector ,@(map
		 (lambda (slot)
		   `(vector-ref match ,slot))
		 (vector->list slots)))
      (vector-ref template-syntax-vector ,rule-index))))

;; What about constants and ellipses? (4 ...) ?
(define (compile-subtemplate template-syntax variables depth)
  (define template (syntax-datum template-syntax))
  (cond
   ((identifier? template)
    (cond
     ((ellipsis? template)
      (compile-error "extraneous ellipsis in template" template-syntax))
     ((map-ref/default variables template #f)
      => (lambda (variable)
	   (cond
	    ((> (pattern-variable-depth variable) depth)
	     (compile-error "pattern variable followed by too few ellipses"
			    template-syntax))
	    ((< (pattern-variable-depth variable) depth)
	     (compile-error "pattern variable followed by too many ellipses"
			    template-syntax))
	    (else	   
	     (values
	      (vector (pattern-variable-index variable))
	      `(lambda (match template-syntax)
		 (derive-syntax (vector-ref match 0)
				template-syntax       
				syntax)))))))
     (else
      (values
       #()
       `(lambda (match template-syntax)
	  (derive-syntax (rename (syntax-datum template-syntax))
			 template-syntax
			 syntax))))))
   ((null? template)
    (values #() `(lambda (match template-syntax) (derive-syntax '() template-syntax syntax))))
   ((pair? template)
    (if (and (list? template) (= (length template) 2) (ellipsis? (syntax-datum (car template))))
	(parameterize ((current-ellipsis? (lambda (identifier) #f)))
	  (let-values (((slots transcriber)
		       (compile-subtemplate (cadr template) variables depth)))
	    (values
	     slots
	     `(lambda (match template-syntax)
		(,transcriber match (cadr (syntax-datum template-syntax)))))))
	(compile-list-template template-syntax variables depth)))
   ((vector? template)
    (let-values
	(((slots transcriber)
	  (compile-list-template (derive-syntax (vector->list template) template-syntax)
				 variables
				 depth)))
      (values
       slots
       `(lambda (match template-syntax)
	  (let ((output
		 (,transcriber match (derive-syntax (vector->list (syntax-datum template-syntax))
						    template-syntax))))
	    (derive-syntax (list->vector (syntax-datum output)) output))))))
   ((constant? template)
    (values
     #()
     `(lambda (match template-syntax)
	(derive-syntax ,template template-syntax syntax))))
   (else
    (compile-error "invalid subtemplate" template-syntax))))

(define (make-template-element template-syntax repeated? index)
  (vector template-syntax repeated? index))
(define (template-element-syntax template-element)
  (vector-ref template-element 0))
(define (template-element-repeated? template-element)
  (vector-ref template-element 1))
(define (template-element-index template-element)
  (vector-ref template-element 2))
(define (analyze-template-list list)
  (let loop ((list list) (%template-element* '()) (index 0))
    (cond
     ((null? list)
      (values (reverse %template-element*) '()))
     ((pair? list)
      (let ((template-syntax (car list)))
	(if (and (pair? (cdr list)) (ellipsis? (syntax-datum (cadr list))))
	    (loop (cddr list)
		  (cons (make-template-element template-syntax #t index)
			%template-element*)
		  (+ index 2))
	    (loop (cdr list)
		  (cons (make-template-element template-syntax #f index)
			%template-element*)
		  (+ index 1)))))
     (else
      (values (reverse %template-element*) `(,(make-template-element list #f index)))))))
(define (make-subtranscriber slots transcriber)
  (vector slots transcriber))
(define (subtranscriber-slots subtranscriber)
  (vector-ref subtranscriber 0))
(define (subtranscriber-transcriber subtranscriber)
  (vector-ref subtranscriber 1))
(define (make-slots+slot-table subtranscriber*)
  (define table (make-table (make-eqv-comparator)))
  (define index 0)
  (define %slot* '())
  (for-each
   (lambda (subtranscriber)
     (vector-for-each
      (lambda (slot)
	(table-intern! table slot (lambda ()
				    (set! %slot* (cons slot %slot*))				    
				    (set! index (+ index 1))
				    (- index 1))))
      (subtranscriber-slots subtranscriber)))
   subtranscriber*)
  (values (list->vector (reverse %slot*)) table))

(define (compile-list-template template-syntax variables-map depth)
  (define template (syntax-datum template-syntax))
  (define-values (template-element* template-element-rest*)
    (analyze-template-list template))
  (define (template-element-compile template-element)
    (define-values (slots transcriber)
      (compile-subtemplate (template-element-syntax template-element)
			   variables-map
			   (if (template-element-repeated? template-element)
			       (+ depth 1)
			       depth)))
    (make-subtranscriber slots transcriber))
  (define subtranscriber* (map-in-order template-element-compile template-element*))
  (define subtranscriber-rest* (map-in-order template-element-compile template-element-rest*))
  (define-values (slots slot-table)
    (make-slots+slot-table (append subtranscriber-rest* subtranscriber*)))
  (define (gen-subtranscriber-call template-element subtranscriber)
    (if
     (template-element-repeated? template-element)
     ;; Repeated template element
     (error "FIXME: Implementation missing")
     ;; Regular template element
     `(,(subtranscriber-transcriber subtranscriber)
       (vector ,@(map
		  (lambda (slot)
		    `(vector-ref match ,(table-ref slot-table slot)))
		  (vector->list (subtranscriber-slots subtranscriber))))
       (vector-ref template-syntax-vector
		   ,(template-element-index template-element)))))
  (define transcriber `
    (lambda (match template-syntax)
      (let* ((template (syntax-datum template-syntax))
	     (template-syntax-vector
	      (list->vector (append (drop-right template 0)
				    (let ((rest (take-right template 0)))
				      (if (null? rest) rest (list rest))))))
	     (output
	      (cons*
	       ,@(map-in-order gen-subtranscriber-call template-element* subtranscriber*)
	       ,(if (null? template-element-rest*)
		    ''()
		    (gen-subtranscriber-call (car template-element-rest*)
					     (car subtranscriber-rest*))))))
	(derive-syntax output template-syntax syntax))))
  (values slots transcriber))

(define (constant? datum)
  (or (char? datum) (string? datum) (boolean? datum) (number? datum) (bytevector? datum)
      (vector? datum)))

(define (analyze-pattern-list pattern-list)
  ;; Return four values.
  ;; P... Q <ellipsis> R ... . S
  ;; gives (P ...) (Q) (R ...) (S)
  (let loop ((pattern-list pattern-list) (first '()))
    (cond
     ((null? pattern-list)
      (values (reverse first) (list) (list) (list)))
     ((pair? pattern-list)
      (cond
       ((ellipsis? (syntax-datum (car pattern-list)))
	(when (null? first)
	  (compile-error "ellipsis is not preceded by a pattern" (car pattern-list)))
	(let loop ((pattern-list (cdr pattern-list))
		   (first (reverse (cdr first)))
		   (ellipsis (list (car first)))
		   (second '()))
	  (cond
	   ((null? pattern-list)
	    (values first ellipsis (reverse second) (list)))
	   ((pair? pattern-list)
	    (if (ellipsis? (syntax-datum (car pattern-list)))
		(compile-error "extraneous ellipsis" (syntax-datum (car pattern-list)))
		(loop (cdr pattern-list) first ellipsis (cons (car pattern-list) second))))
	   (else
	    (values first ellipsis (reverse second) (list pattern-list))))))
       (else
	(loop (cdr pattern-list) (cons (car pattern-list) first)))))
     (else
      (values (reverse first) (list) (list) (list pattern-list))))))
