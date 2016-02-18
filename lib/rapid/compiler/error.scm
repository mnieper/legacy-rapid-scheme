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
  (make-compile-error-object type message syntax)
  compile-error-object?
  (type compile-error-object-type)
  (message compile-error-object-message)
  (syntax compile-error-object-syntax))

;;; Notes

(define (make-note message syntax) (vector message syntax))
(define (note-message note) (vector-ref note 0))
(define (note-syntax note) (vector-ref note 1))
(define current-notes (make-parameter '() #f))
(define (%get-notes) (unbox (current-notes)))
(define (get-notes!)
  (define notes (reverse (%get-notes)))
  (set-box! (current-notes) '()))
(define (append-note! message syntax)
  (set-box! (current-notes) (cons (make-note message syntax) (%get-notes))))

(define (compile-error message syntax)
  (raise (make-compile-error-object 'error message syntax)))
(define (compile-warning message syntax)
  (raise-continuable (make-compile-error-object 'warning message syntax)))
(define (compile-note message syntax)
  (append-note! message syntax))

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

(define (display-context syntax)
  (do ((syntax syntax (syntax-context syntax)))
      (syntax)
    (display-source-location (syntax-source-location syntax))
    (display "  used from here")
    (newline)))

(define (display-compile-error compile-error-object)
  (define syntax (compile-error-object-syntax compile-error-object))
  (parameterize ((current-output-port (current-error-port)))
    (when syntax (display-source-location (syntax-source-location syntax)))
    (case (compile-error-object-type compile-error-object)
      ((error) (display "error: "))
      ((warning) (display "warning: "))
      (else => (lambda (type) (error "invalid compile error type" type))))
    (display (compile-error-object-message compile-error-object))
    (newline)
    (display-context syntax)
    (do ((notes (get-notes!) (cdr notes)))
	((null? notes))
      (let* ((note (car notes)) (syntax (note-syntax note)))
	(display-source-location (syntax-source-location syntax))
	(display "note: ")
	(display (note-message note))
	(newline)
	(display-context syntax)))))

(define-syntax guard-compile
  (syntax-rules ()
    ((guard-compile body1 body2 ...)
     (parameterize ((current-notes '()))
       (guard (condition
	       (cond
		((compile-warning? condition)
		 (display-compile-error condition))
		((compile-error? condition)
		 (display-compile-error condition)
		 (exit #f))
		(else
		 (display "compiler error" (current-error-port))
		 (newline (current-error-port))
		 (raise condition))))
	 body1
	 body2
	 ...)))))
