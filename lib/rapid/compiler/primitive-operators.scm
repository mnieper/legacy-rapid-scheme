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

;; Equivalence predicates
(define operator-eq? (make-operator 'eq?))

;; Fixnums
(define operator-fixnum? (make-operator 'fixnum?))
(define operator-fxnegative? (make-operator 'fxnegative?))
(define operator-fx< (make-operator 'fx<))
(define operator-fx+ (make-operator 'fx+))

;; Flonums
(define operator-flonum? (make-operator 'flonum?))
(define operator-nan? (make-operator 'nan?))

;; Exact numbers
(define operator-exact? (make-operator 'exact?))

;; Pairs
(define operator-cons (make-operator 'cons))
(define operator-car (make-operator 'car))
(define operator-cdr (make-operator 'cdr))
(define operator-pair? (make-operator 'pair?))
(define operator-null? (make-operator 'null?))

;; Strings
(define operator-string? (make-operator 'string?))

;; Vectors
(define operator-make-vector (make-operator 'make-vector))
(define operator-vector-ref (make-operator 'vector-ref))
(define operator-vector-set! (make-operator 'vector-set!))
(define operator-vector? (make-operator 'vector?))

;; Control features
(define operator-apply (make-operator 'apply))
(define operator-call-with-current-continuation (make-operator 'call-with-current-continuation))



;; XXX

(define operator+
  (make-operator '+))


(define operator-display
  (make-operator 'display))

(define operator-newline
  (make-operator 'newline))

(define operator-string-append
  (make-operator 'string-append))
