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

(define (expand-program syntax*)
  (define-values (body environment)
    (let loop ((syntax* syntax*) (import-sets '()))
      (if (null? syntax*)
	  (values '() (expand-import-sets (reverse import-sets)))
	  (let ((form (syntax-datum (car syntax*))))
	    (if (and (list? form) (>= (length form) 1) (eq? (syntax-datum (car form)) 'import))
		(loop (cdr syntax*) (append (cdr form) import-sets))
		(values syntax* (expand-import-sets (reverse import-sets))))))))
  (define bindings (environment-bindings environment))
  (with-syntactic-environment
   (environment-syntactic-environment environment)
   (lambda ()
     (make-letrec*-expression
      (append (environment-bindings environment)
	      (with-scope
	       (lambda ()
		 (expand-top-level body))))
      (list (make-literal #t #f))
      #f))))
