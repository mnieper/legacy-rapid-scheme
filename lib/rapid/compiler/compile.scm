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

(define (compile filename)
  ;; TODO  (use expression->datum for the moment) and output
  ;; CALL EXPAND-PROGRAM AND THEN THE REST
  
  (define read-syntax (read-file (datum->syntax filename) #f))
  (define-values (body environment)
    (let loop ((import-sets '()))
      (define syntax (read-syntax))
      (if (eof-object? syntax)
	  (values '() (make-environment (reverse import-sets)))
	  (if (and (list? form) (>= (length form) 1) (eq? (syntax-datum (car form)) 'import))
	      (loop (append (cdr form) import-sets))
	      (values (cons syntax (generator->list read-syntax))
		      (make-environment (reverse import-sets)))))))
  (define gensym (environment-gensym environment))
  (define-values (top-level-bindings syntactic-environment)
    (expand body (environment-syntactic-environment environment) gensym))
  (define program
    `(letrec* ,(append (environment-bindings environment) top-level-bindings)
	      #t))
  ;; TODO: simplify program
  ;; TODO: compile program
  program)
