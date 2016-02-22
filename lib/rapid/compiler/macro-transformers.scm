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

;; TODO: Use syntax objects
(define (make-er-macro-transformer transformer macro-environment)
  (lambda (form environment)
    (define renames (make-table (make-eq-comparator)))
    (define (rename identifier)
      (table-intern! renames
		     identifier
		     (lambda ()
		       (make-syntactic-closure macro-environment '() identifier))))
    (define (compare identifier1 identifier2)
      (identifier=? environment identifier1 environment identifier2))
    (transformer form rename compare)))


;; TODO: Use syntax objects
(define (make-syntax-rules-transformer ellipsis literal* syntax-rule* macro-environment)
  (define er-macro-transformer
    (eval (compile-syntax-rules-transformer ellipsis literal* syntax-rule*)
	  *transformer-environment*))
  (make-er-macro-transformer er-macro-transformer macro-environment))

(define (compile-syntax-rules-transformer ellipsis literal* syntax-rule*)
					;... clauses ...
  (define clauses '()) ;; TODO
  `(lambda (form rename compare)
     (cond
      ,@clauses
      (else (error "no expansion for" form))))) ;; TODO: compile-error, syntax?
