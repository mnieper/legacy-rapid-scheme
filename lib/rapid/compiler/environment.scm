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

(define-record-type <environment>
  (make-environment bindings syntactic-environment)
  environment?
  (bindings environment-bindings)
  (syntactic-environment environment-syntactic-environment))

(define-syntax environment
  (syntax-rules ()
    ((environment ((formals init) ...) syntactic-bindings)
     (environment-aux (formals ...) () ((formals init) ...) syntactic-bindings))))

(define-syntax environment-aux
  (syntax-rules ()
    ((environment-aux () location-bindings environment-bindings syntactic-bindings)
     (let location-bindings
       (make-environment
	 (bindings . environment-bindings)
	 (syntactic-environment . syntactic-bindings))))
    ((environment-aux ((x ...) formals ...)
		      (location-binding ...)
		      environment-bindings
		      syntactic-bindings)
     (environment-aux (formals ...)
		      (location-binding ... (x (make-location #f)) ...)
		      environment-bindings
		      syntactic-bindings))
    ((environment-aux ((x ... . y) formals ...)
		      (location-binding ...)
		      environment-bindings
		      syntactic-bindings)
     (environment-aux (formals ...)
		      (location-binding ... (x (make-location #f)) ... (y (make-location #f)))
		      environment-bindings
		      syntactic-bindings))))

;;; BINDINGS REFER TO EXPRESSIONS... THEY INTRODUCE LOCATIONS BOUND TO SCHEME VARS
;;; SYNTACTIC BINDINGS REFER TO TRANSFORMERS IN THE GENERAL SENSE


;; (TODO syntactic-environment, bindings-macro in the specific libraries

;;  ((&+ (make-lambda (...) (&+ xxx xxx xxx))))   ;; makes x a location; stores x there...
;;  (+ (HIER KOMMT EBENFALLS WAS HIN, verweist auf &+)
;;  %syntactic-env)

;; So, what is a transformer?

;; -> what is the expansion if used as a value?
;; -> If used as keyword:
;;    -> receives complete form and current syntactic-environment
;;    -> should return the expanded form as an expression
;;
;; However:
;;    -> could introduce bindings (as (internal) defines)
;;    -> more than one line (as begin ...)
;;    -> need to handle lambda body
;;
;; -> what in (a b c)?  Here, a is a procedure... is okay
;;    -> (lambda (x) ... x) introduces "references". These references are transformers themselves
;;
;; What are the primitive expression types?
;; -> reference
;; -> quote
;; -> procedure calls (handled analoguously to macro use)
;; -> lambda, if, set! (set sets flag)
;; -> include... (see above)
;;
;; -> internal defines

;; A denotation is a transformer together with a value (or false)
;; A transformer is a procedure that receives the form and a syntactic-environment
;;
;; A transformer like ‘begin’ needs the expander that can be fed... the same goes for include
;; return binding, when define...
;;
;; Where to record references à la capture-references? whenever we expand something in the
;; expander. When bindings are returned: check any taints...
;;
;; Problem:
;; (let () (begin (define ...) exp1 exp2)) ... I am allowed to change exp2...
;; ... this means we can step out of recording mode for the let body after exp1...
;; ... however, we are not allowed to define anything afterwards anyway... :-)

