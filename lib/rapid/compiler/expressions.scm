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

(define-record-type <expression>
  (make-expression type value syntax)
  expression?
  (type expression-type)
  (value expression-value)
  (syntax expression-syntax))

(define (make-location syntax)
  (make-expression 'location #f syntax))
(define (location? expression)
  (eq? (expression-type expression) 'location))

(define-record-type <bindings>
  (%make-bindings binding* syntax)
  bindings?
  (binding* %bindings-binding*)
  (syntax bindings-syntax))
(define (make-bindings syntax)
  (%make-bindings '() syntax))
(define (bindings->list bindings)
  (reverse (%bindings-binding* bindings)))
(define (bindings-insert bindings binding syntax)
  (%make-bindings (cons binding (%bindings-binding* bindings))))
(define (bindings-append bindings1 bindings2)
  (%make-bindings (append (%bindings-binding* bindings2)
			      (%bindings-binding* bindings1))))

(define-record-type <binding>
  (make-binding formals init syntax)
  binding?
  (formals binding-formals)
  (init binding-init)
  (syntax binding-syntax))

(define-record-type <formals>
  (%make-formals fixed-arguments rest-argument syntax)
  formals?
  (fixed-arguments formals-fixed-arguments)
  (rest-argument formals-rest-argument)
  (syntax formals-syntax))

(define make-formals
  (case-lambda
   ((fixed-arguments syntax) (make-formals fixed-arguments #f syntax))
   ((fixed-arguments rest-argument syntax) (%make-formals fixed-arguments rest-argument syntax))))

(define-syntax bindings
  (syntax-rules ()
    ((bindings (formals expression) ...)
     (bindings-aux ((formals expression) ...) ()))))

(define-syntax bindings-aux
  (syntax-rules ()
    ((bindings-aux () ((formals expression) ...))
     (%make-bindings
      `(,(make-binding formals expression #f) ...)
      #f))
    ((bindings-aux (((x ...) expression) binding2 ...) reversed-bindings)
     (bindings-aux (binding2 ...) (((make-formals `(,x ...) #f) expression) . reversed-bindings)))
    ((bindings-aux (((x ... . y) expression) binding2 ...) reversed-bindings)
     (bindings-aux (binding2 ...)
		   (((make-formals `(,x ...) y #f) expression) . reversed-bindings)))))
