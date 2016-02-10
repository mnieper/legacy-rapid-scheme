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

(define-record-type <compile-error-object>
  (make-compile-error-object type messages)
  compile-error-object?
  (type compile-error-object-type)
  (messages compile-error-object-messages))

(define-syntax compile-error
  (syntax-rules ()
    ((compile-error (syntax (message obj ...)) ...)
     (raise (make-compile-error-object 'error
				       (list (cons syntax (format message obj ...)) ...))))))
		    
(define-syntax compile-warning
  (syntax-rules ()
    ((compile-warning (syntax (message obj ...)) ...)
     (raise (make-compile-error-object 'warning
				       (list (cons syntax (format message obj ...)) ...))))))

(define (compile-error? object)
  (and (compile-error-object? object)
       (eq? (compile-error-object-type object) 'error)))

(define (compile-warning? object)
  (and (compile-error-object? object)
       (eq? (compile-error-object-type object) 'warning)))

(define (display-source-location source-location)
  (define source (source-location-source source-location))
  (when source
    (display source)
    (display ": ")
    (display (source-location-start-line source-location))
    (display ".")
    (display (+ (source-location-start-column source-location) 1))
    (display "-")
    (unless (= (source-location-start-line source-location)
	       (source-location-end-line source-location))
      (display (source-location-end-line source-location))
      (display "."))
    (display (+ (source-location-end-column source-location) 1))
    (display ": ")))

(define (display-compile-error compile-error-object)
  (define type (compile-error-object-type compile-error-object))
  (define messages (compile-error-object-messages compile-error-object))
  (let loop ((type type) (messages messages))
    (unless (null? messages)
      (let ((syntax (caar messages)) (text (cadr messages)))
	(display-source-location (syntax-source-location syntax))
	(case type
	  ((error) (display "error: "))
	  ((warning) (display "warning: "))
	  ((note) (display "note: "))
	  (else (error "display-compile-error: invalid compile error type" type)))
	(display text)
	(newline)
	(do ((syntax (syntax-context syntax) (syntax-context syntax)))
	    (syntax)
	  (display-source-location (syntax-source-location syntax))
	  (display "  used from here")
	  (newline))))))
