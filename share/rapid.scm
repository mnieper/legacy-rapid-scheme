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

;;; TODO: Check syntax (for %define-syntax, etc.)
;;; XXX: Check whether quote/quasiquote have to imported and re-imported from rapid macros

(define-macro m-define-syntax ... (syntax-rules)
  ((define-syntax 'keyword '(syntax-rules (literal ...) '(pattern template) ...))
   (m-define-syntax-aux 'keyword '() (literal ...) '((pattern template) ...)))
  ((define-syntax 'keyword '(syntax-rules ellipsis (literal ...) '(pattern template) ...))
   (m-define-syntax-aux 'keyword '(ellipsis) '(literal ...) '((pattern template) ...))))

(define-macro m-define-syntax-aux ... ()
  ;; TODO: more complicated than this!
  ((m-define-syntax-aux 'keyworkd 'ellipsis* 'literal* 'syntax-rule*)
   `(%define-syntax 'keyword
     (syntax-rules ,(m-car 'ellipsis*) 'literal* . 'syntax-rule*))))

#|			
(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda clause ...)
     (case-lambda-aux (clause ...) ()))))

(define-syntax case-lambda-aux
  (syntax-rules ()
    ((case-lambda-aux () (clause ...))
     (%case-lambda-aux clause ...))
    ((case-lambda-aux (clause1 clause2 ...) (checked-clause ...))
     (case-lambda-aux (clause2 ...) (checked-clause ... clause1)))))

(define-syntax lambda
  (syntax-rules ()
    ((lambda formals . body)
     (case-lambda (formals . body)))
    ((lambda . args)
     (syntax-error "bad lambda form"))))

;; TODO Check formals (also elsewhere)  ;; with cps-macros (?)
(define-syntax define-values
  (syntax-rules ()
    ((define-values formals expression)
     (%define-values formals expression))
    ((define-values . args)
     (syntax-error "bad define-values form"))))

(define-syntax define
  (syntax-rules ()
    ((define (variable . formals) . body)
     (define variable (lambda formals . body))) 
    ((define variable expression)
     (define-values (variable) expression))
    ((define . _)
     (syntax-error "bad define form"))))

(define-syntax quote
  (syntax-rules ()
    ((quote datum)
     (%quote datum))
    ((quote . args)
     (syntax-error "bad quote form"))))
 
|#
