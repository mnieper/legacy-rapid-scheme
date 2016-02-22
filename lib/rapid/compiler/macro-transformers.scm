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

#;(define (eval-transformer transformer)
  ((eval `(lambda (compile-error format syntax->datum)
	   ,transformer)
	*transformer-environment*)
   compile-error format syntax->datum))

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


;; TODO: Use syntax objects for the input of make-syntax-rules-transformer
(define (make-syntax-rules-transformer ellipsis literal* syntax-rule* transformer-syntax
				       macro-environment)
  ;; TODO: add transformer-syntax to environment
  ;; We need an easy way to add extra parameter to lambda...
  (define er-macro-transformer
    (eval-transformer (compile-syntax-rules-transformer ellipsis literal* syntax-rule*)
		      compile-error compile-note transformer-syntax))
  (make-er-macro-transformer er-macro-transformer macro-environment))

(define (compile-syntax-rules-transformer ellipsis literal* syntax-rule*)
  (define clauses '()) ;; TODO
  `(lambda (syntax rename compare)
     (cond
      ,@clauses
      (else
       (compile-note "the macro definition is here" transformer-syntax)
       (compile-error "no expansion for macro use" syntax)))))
