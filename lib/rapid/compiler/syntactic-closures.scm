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

;;; Data types

(define-record-type <syntactic-closure>
  (make-syntactic-closure environment free-names form)
  syntactic-closure?
  (environment syntactic-closure-environment)
  (free-names syntactic-closure-free-names)
  (form syntactic-closure-form))

(define (close-syntax form environment)
  (make-syntactic-closure environment '() form))

(define (identifier? form)
  (or (symbol? form)
      (and (syntactic-closure? form)
	   (identifier? (syntactic-closure-form form)))))

(define (make-synthetic-identifier identifier)
  (close-syntax identifier #f))

(define (identifier=? environment1 identifier1 environment2 identifier2)
  'TODO
  )



;;;

(define (call-in-syntactic-closure syntactic-closure proc)
  (let ((syntactic-environment (get-syntactic-environment)))
    (with-syntactic-environment
     (or (syntactic-closure-environment syntactic-closure)
	 (make-syntactic-closure))
     (lambda ()
       (with-scope
	(lambda ()
	  ;; ADD FREE BINDINGS
	  (proc (syntactic-closure-form syntactic-closure))))))))

(define (lookup-denotation identifier environment)
  (if (symbol? identifier)
      ;; Symbol
      (with-syntactic-environment
       environment
       (lambda ()
	 (lookup-denotation! identifier)))
      ;; Syntactic closure
    ))  

;;;

