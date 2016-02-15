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


;; is denotation a good name?
(define begin-denotation
  ;; Transformer
  (lambda (form-syntax syntactic-environment expander)
    ;; returns two values: an expansion and/or binding
    ...))

;; A general denotation consists of a value and a transformer.
;; Alternatively: transformer gets "as value" or "as combination" flag...
;; for example: whether the form is a list or an identifier...

;; Transformer needs form (*)
;; needs syntactic-env (okay)
;; needs expander: to expand the resulting;;; q: expander: should we able to wire-through something?
;; returns with something: syntactic-binding (?); expression (?); new binding/dummy (?) 
;;
;; (define define 3) <--- bei macro-expand machen wir capture-references (Im Hauptteil)
;; bis er expander aufruft?
3
(define (expand body syntactic-environment top-level?)  ;; can expand body or top-level
  (let loop ((body body) (bindings (make-bindings)) (syntactic-environment syntactic-environment))
    (define (expand-command-or-defintion command-or-definition command-or-definition*)
      (define (insert-expression expression)
	(loop command-or-definition*
	      (bindings-insert bindings (make-identifier #f) expression command-or-definition)))
      (define form (syntax-datum command-or-definition))
      (cond
       ((simple-datum? form)
	(insert-expression
	 (make-constant form syntax))) ; make-constant in expressions
       ((null? form)
	(compile-error "empty application in source" command-or-definition))
       ((symbol? form)
	(cond
	 ((assq (lookup-binding form syntactic-environment))
	  => (lambda (binding)
	       (insert-expression ((binding-denotation binding) form syntactic-environment))))
	 (else
	  (compile-error "undefined variable" command-or-definition))))
       ((and-let* ((list? form)
		   ((f (syntax-datum (car form))))
		   (symbol? f))
	  f)
	=> (lambda (f)
	     (cond
	      ((assq (lookup-binding f syntactic-environment))
	       => (lambda (binding)
		    (capture-references
		     (lambda () ((binding-denotation binding) form syntactic-environment))
		     (lambda (expansion)
		       (cond
			((definition? expansion)
			 ; check whether referenced
			 ...)
			((...)
			 ...))
		       )))))
	
	
	(let ()
	  (define operator (syntax-datum (car form)))
	  (if (symbol? operator)
	      (lookup transformer ...)
	      (make-application expand everything)
	      
	
	;; expand first part as expression (?) jedenfalls nicht toplevel
	;; call transformer of all...
	;;
	;; landet wieder hier... und dann?
	;; rufe nochmal auf...
	;; wie lange? bis primitive gefunden...
	;; besser: der expander ruft selbst auf... was ruft er auf? den übergebenen?
	
	...)
       ((symbol? form)
	...)
       (else (compile-error "invalid form"))))
    (if (null? body)
	(make-environment bindings syntactic-environment)
	(expand-command-or-definition (car body) (cdr body)))))

;; Was ist mit:
;;
;; ((begin a b c) ...)
;;
;; In diesem Falle ist (begin ...) ein Ausdruck...
;; Damit muß ‘begin’ von (rapid primitive) wissen, wo es eingesetzt wird... also den context
;; kennen...
;;
;; wir könnten auch mit parameterize arbeiten...
;; dann müßten wir dem Transformer nicht so viel übergeben...
;; parameterized werden müßte:
;; -> define-values
;; -> begin
;; -> expression
;; -> define-syntax
;; (+ x y)
;; + 

(define initial-environment
  (environment
   ;; Bindings
   ((... ...)
    (... ...))
   ;; Syntactic environment
   (if if-transformer)
   (begin begin-transformer)
   (define-values define-values-transformer)))

(define if-transformer
  (lambda (form syntactic-environment
		
