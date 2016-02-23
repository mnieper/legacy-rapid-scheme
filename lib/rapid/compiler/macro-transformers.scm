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

(define *transformer-environment* (environment '(scheme base)))

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

(define (make-syntax-rules-transformer ellipsis
				       literal*
				       syntax-rule-syntax*
				       transformer-syntax
				       macro-environment)
  (define er-macro-transformer
    (eval-transformer (compile-syntax-rules-transformer ellipsis
							literal*
							syntax-rule-syntax*)
		      compile-error compile-note transformer-syntax
		      syntax-datum derive-syntax))
  (make-er-macro-transformer er-macro-transformer macro-environment))

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

(define (compile-syntax-rules-transformer ellipsis literal* syntax-rule-syntax*)
  (define clauses
    (map-in-order
     (lambda (syntax-rule-syntax)
       (define syntax-rule (syntax-datum syntax-rule-syntax))
       (unless (and (list? syntax-rule) (= (length syntax-rule) 2))
	 (compile-error "bad syntax-rule" syntax-rule-syntax))
       (let*-values
	   (((pattern-syntax) (car syntax-rule))
	    ((template-syntax) (cadr syntax-rule))
	    ((identifiers matcher) (compile-pattern pattern-syntax ellipsis literal*))
	    ((transcriber) (compile-template template-syntax identifiers ellipsis literal*)))
	 `(,matcher => ,transcriber)))
     syntax-rule-syntax*))
  `(lambda (syntax rename compare)
     (define form (syntax-datum syntax))
     (cond
      ,@clauses
      (else
       (compile-note "the macro definition is here" transformer-syntax)
       (compile-error "no expansion for macro use" syntax)))))

(define (make-pattern-variable index depth) (vector index depth))
(define (pattern-variable-index variable) (vector-ref variable 0))
(define (pattern-variable-depth variable) (vector-ref variable 1))

(define (compile-pattern pattern-syntax ellipsis literal*)
  (define pattern (syntax-datum pattern-syntax))
  (unless (and (list? pattern) (>= (length pattern) 1))
    (compile-error "invalid pattern" pattern-syntax))
  (let-values (((identifiers matcher)
		(compile-subpattern (derive-syntax (cdr pattern) pattern-syntax)
				    ellipsis
				    literal*)))
    (values identifiers `(,matcher (cdr form)))))

(define (make-pattern-variable-map) (make-map (make-eq-comparator)))

(define (compile-subpattern pattern-syntax ellipsis literal*)
  (define pattern (syntax-datum pattern-syntax))
  (cond
   ((identifier? pattern)
    (cond
     ((memq pattern literal*) ;; TODO: use sets
      (values
       (make-pattern-variable-map)
      `(lambda (form)
	 (and (compare form pattern) #()))))
     ((eq? pattern '_)
      (values
       (make-pattern-variable-map)
       `(lambda (form)
	  #())))
     (else
      (values
       (map-set (make-pattern-variable-map) pattern (make-pattern-variable 0 0))
       `(lambda (form)
	  (vector form))))))
   ((null? pattern)
    (values
     (make-pattern-variable-map)
     `(lambda (form)
	(null? form))))
   (else
    (compile-error "invalid subpattern" pattern-syntax))))

(define (compile-template template-syntax variables ellipsis literal*)
  (define-values (slots transcriber)
    (compile-subtemplate template-syntax variables ellipsis literal*))
  `(lambda (match)
     (,transcriber
      (vector ,@(map
		 (lambda (slot)
		   `(vector-ref match ,slot))
		 (vector->list slots))))))

(define (compile-subtemplate template-syntax variables ellipsis literal*)
  (define template (syntax-datum template-syntax))
  (cond
   ((identifier? template)
    (cond
     ((map-ref/default variables template #f)
      => (lambda (variable)
	   (values
	    (vector (pattern-variable-index variable))
	    `(lambda (match)
	       (derive-syntax (vector-ref match 0)
			      template-syntax       
			      syntax)))))
     (else
      (values
       #()
       `(lambda (match)
	  (derive-syntax (rename ',template)
			 template-syntax
			 syntax))))))
   (else
    (compile-error "invalid subtemplate" template-syntax))))
