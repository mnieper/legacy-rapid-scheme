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

(define (make-syntax-expander expander)
  (lambda (syntax)
    (unless (list? (syntax-datum syntax))
      (compile-error "invalid use of syntax as value" syntax))
    (expander syntax)))

(define begin-expander
  (make-syntax-expander
   (lambda (syntax)
     (insert-sequence! (cdr (syntax-datum syntax))))))

(define if-expander
  (make-syntax-expander
   (lambda (syntax)
     (define form (syntax-datum syntax))
     (unless (or (= (length form) 2) (= (length form 3)))
       (compile-error "bad if syntax" syntax))
     (let ((test-syntax (list-ref form 1))
	   (consequent-syntax (list-ref form 2))
	   (alternate-syntax (and (= (length form 3)) (list-ref form 3))))     
       (make-expression-expander
	(lambda ()
	  (make-conditional
	   (expand-expression test-syntax)
	   (expand-expression consequent-syntax)
	   (if alternate-syntax
	       (expand-expression alternate-syntax)
	       (make-constant #f #f))
	   syntax)))))))

(define (make-primitive-procedure operator)
  (make-case-lambda ...))

(define (make-primitive-expander operator reference)
  (lambda (syntax)
    (define form (syntax-datum syntax))
    (if (symbol? form)
	(make-expression-expander reference)
	(make-expression-expander
	 (lambda ()
	   (make-primitive-call operator syntax))))))
   
(define initial-environment
  (environment
   ;; Bindings
   (((%+) (make-primitive-procedure '+))
    )
   ;; Syntactic environment
   (begin begin-expander)
   (if if-expander)
   (+ (make-primitive-expander '+ '%))
   
   ))
