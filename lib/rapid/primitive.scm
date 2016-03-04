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

;; Fundamental binding construct

(define-syntax letrec*-values
  (syntax-rules ()
    ((letrec*-values ((formals init) ...) body1 body2 ...)
     (let ()
       (define-values formals init)
       ...
       (let ()
	 body1
	 body2
	 ...)))))

;; Numbers

(define (fixnum? obj)
  (exact-integer? obj))

(define (flonum? obj)
  (and (real? obj) (inexact? obj)))

(define (fx+ n1 n2)
  (+ n1 n2))

(define (fx< n1 n2)
  (< n1 n2))

(define (fxnegative? n)
  (negative? n))

;; Error objects

(define (make-error-object message obj*)
  (guard
      (condition
       (else condition))
    (apply error message obj*)))
