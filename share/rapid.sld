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
   ;; Iteration
   do
   ;; Binding constructs
   let let* letrec letrec* let-values let*-values
   ;; Control features
   values dynamic-wind call-with-current-continuation
   ;; Parameter objects
   make-parameter parameterize
   ;; Record-type definitions
   define-record-type
   ;; Equivalence predicates
   eq?
   ;; Booleans
   boolean? not boolean=?
   ;; Symbols
   symbol?
   ;; Lists
   null? cons pair? car cdr list caar cadr cdar cddr
   length
   ;; Characters
   char? char=?
   ;; Strings
   string?
   string->list
   list->string
   ;; Vectors
   vector? make-vector vector-length vector-ref vector-set! vector list->vector
   ;; Control features
   procedure?
   map
   for-each
   string-for-each
   ;; Exceptions
   with-exception-handler raise raise-continuable error
   ;; Output
   display
   newline
   ;; Quasiquotation
   unquote unquote-splicing quasiquote
   ;; Process context
   exit
   ;; Formatting
   format

   ;; XXX
   +
   )
  (import (rename (prefix (rapid primitive) %)

		  ;; XXX
		  (%+ +)
		  
		  (%_ _)
		  (%... ...)
		  (%syntax-rules syntax-rules)
		  (%define-syntax define-syntax)
		  (%define-values define-values)
		  (%quote quote)
		  (%set! set!)
		  (%if if)
		  (%begin begin)
		  (%syntax-error syntax-error)))
  (include "rapid.scm"))
