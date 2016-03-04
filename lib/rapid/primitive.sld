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

(define-library (rapid primitive)
  (export letrec*-values
	  quote
	  case-lambda
	  if
	  set!
	  begin

	  eq?

	  fixnum?
	  flonum?
	  exact?
	  nan?

	  fx+
	  fx<
	  fxnegative?
	  
	  string?
	  
	  make-vector
	  vector-ref
	  vector-set!
	  vector?
	  
	  cons
	  pair?
	  car
	  cdr
	  null?
	  
	  make-error-object
	  call-with-current-continuation
	  apply
	  
	  ;; XXX	  
	  display newline string-append)
  (import (scheme base)
	  (scheme inexact)
	  (only (scheme write) display newline) ;; FIXME
	  (scheme case-lambda))
  (include "primitive.scm"))
