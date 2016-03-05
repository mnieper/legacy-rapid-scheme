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

(define-library (rapid compiler primitive-operators)
  (export
   operator-eq?
   operator-boolean?
   operator-fixnum?
   operator-fxnegative?
   operator-fx<
   operator-fx=
   operator-fx+
   operator-flonum?
   operator-nan?
   operator-exact?
   operator-cons
   operator-pair?
   operator-car
   operator-cdr
   operator-null?
   operator-char?
   operator-string?
   operator-string->list
   operator-make-vector
   operator-vector-ref
   operator-vector-set!
   operator-vector?
   operator-vector-length
   operator-procedure?
   operator-apply
   operator-call-with-current-continuation
   operator-make-error-object
   operator-exit
   operator-make-rtd
   operator-rtd-constructor
   operator-rtd-predicate
   operator-rtd-accessor
   operator-rtd-mutator

   ;; XXX
   operator+
   operator-display operator-newline operator-string-append) ;; TODO
  (import (scheme base)
	  (rapid compiler expressions))
  (include "primitive-operators.scm"))
