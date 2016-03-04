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

(define-library (rapid)
  (export
   ;; Macros
   define-syntax syntax-rules syntax-error ... _
   ;; Literals
   quote
   ;; Procedures and Definitions
   case-lambda lambda define define-values
   ;; Assignment
   set!
   ;; Conditionals
   if else => cond and or when unless
   ;; Binding constructs
   let let* letrec letrec* #;let-values #;let*-values
   ;; Control features
   values dynamic-wind call-with-current-continuation
   ;; Parameter objects
   make-parameter parameterize
   ;; Equivalence predicates
   eq?
   ;; Lists
   null? cons pair? car cdr list caar cadr cdar cddr
   length
   ;; Strings
   string?
   ;; Vectors
   vector? make-vector vector-length vector-ref vector-set! vector list->vector
   ;; Exceptions
   with-exception-handler raise raise-continuable error
   ;; Quasiquotation
   unquote unquote-splicing quasiquote
   )
  (import (rename (prefix (rapid primitive) %)
		  (%_ _)
		  (%... ...)
		  (%syntax-rules syntax-rules)
		  (%define-syntax define-syntax)
		  (%define-values define-values)
		  (%quote quote)
		  (%set! set!)
		  (%if if)
		  (%syntax-error syntax-error)))
  (include "rapid.scm"))
