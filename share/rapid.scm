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

;;; Procedures and Definitions

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (formals body1 body2 ...) ...)
     (%case-lambda
      (formals body1 body2 ...)
      ...
      (args
       (error "procedure called with the wrong number of arguments" args))))
    ((case-lambda . _)
     (syntax-error "bad case-lambda syntax"))))

(define-syntax lambda
  (syntax-rules ()
    ((lambda formals body1 body2 ...)
     (case-lambda
      (formals body1 body2 ...)))
    ((lambda . _)
     (syntax-error "bad lambda syntax"))))

(define-syntax define
  (syntax-rules ()
    ((define (variable . formals) body1 body2 ...)
     (define-values (variable)
       (lambda formals body1 body2 ...)))
    ((define variable expression)
     (define-values (variable) expression))
    ((define . args)
     (syntax-error "bad define syntax"))))

;;; Record-type definitions

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type name
       (constructor-name . field-name*)
       pred
       (field-name accessor-name modifier-name ...) ...)
     (define-record-type-aux name
       (constructor-name . field-name*)
       pred
       ((field-name accessor-name modifier-name ...) ...)
       ()))
    ((define-record-type . _)
     (syntax-error "bad define-record-type syntax"))))

(define-syntax define-record-type-aux
  (syntax-rules ()
    ((define-record-type-aux name
       (constructor-name . field-name*)
       pred
       ()
       ((field-name accessor-name %accessor-name (mutator-name %mutator-name) ...) ...))
     (begin
       (%define-record-type
	name
	(%constructor-name . field-name*)
	%pred
	(field-name %accessor-name %mutator-name ...) ...)
       (define (constructor-name . field-name*)
	 (%constructor-name . field-name*))
       (define (pred obj)
	 (%pred obj))
       (begin
	 (define (accessor-name obj)
	   (unless (%pred obj)
	     (error (format "not a ‘~a’ record" 'name) obj))
	   (%accessor-name obj))
	 (define (mutator-name obj value)
	   (unless (%pred obj)
	     (error (format "not a ‘~a’ record" 'name) obj))
	   (%mutator-name obj value)) ...)
       ...))
    ((define-record-type-aux name constructor pred
       ((field-name accessor-name mutator-name ...) . field*)
       tmps)
     (define-record-type-aux name constructor pred
       field*
       ((field-name accessor-name %accessor-name (mutator-name %mutator-name) ...) . tmps)))))
       
;;; Conditionals

(define-syntax else
  (syntax-rules ()
    ((else . _)
     (syntax-error "invalid use of auxiliary syntax ‘else’"))))

(define-syntax =>
  (syntax-rules ()
    ((else . _)
     (syntax-error "invalid use of auxiliary syntax ‘else’"))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
	   (result temp)
	   (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
	   temp
	   (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
	   clause1 clause2 ...)
     (if test
	 (begin result1 result2 ...)
	 (cond clause1 clause2 ...)))
    ((cond . _)
     (syntax-error "bad cond syntax"))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
	 (begin result1 result2 ... (if #f #f))))
    ((when . _)
     (syntax-error "bad when syntax"))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
	 (begin result1 result2 ... (if #f #f))))))

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
	 (begin result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
	 (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
	 (result key)
	 (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
	 (begin result1 result2 ...)
	 (case key clause clauses ...)))
    ((case . _)
     (syntax-error "bad case syntax"))))

;;; Iteration

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...) (test expr ...) command ...)
     (let loop ((var init) ...)
       (cond
	(test
	 (if #f #f)
	 expr ...)
	(else
	 command ...
	 (loop (do-aux var step ...) ...)))))
    ((do . _)
     (syntax-error "bad do syntax"))))

(define-syntax do-aux
  (syntax-rules ()
    ((do-aux x)
     x)
    ((do-aux x y)
     y)))

;;; Binding constructs

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
		      body1 body2 ...)))
	tag)
      val ...))
    ((let . _)
     (syntax-error "bad let syntax"))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
	 body1 body2 ...)))
    ((let* . _)
     (syntax-error "bad let* syntax"))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((variable init) ...) body1 body2 ...)
     (let ()
       (define-values (variable ...) (values init ...))
       (let ()
	 body1 body2 ...)))
    ((letrec . _)
     (syntax-error "bad letrec syntax"))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec ((variable init) ...) body1 body2 ...)
     (let ()
       (define-values (variable) init) ...
       (let ()
	 body1 body2 ...)))
    ((letrec . _)
     (syntax-error "bad letrec* syntax"))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values ((formals init) ...) body1 body2 ...)
     (let-values-aux ((formals init) ...) () (body1 body2 ...)))
    ((let-values . _)
     (syntax-error "bad let-values syntax"))))
  
(define-syntax let-values-aux
  (syntax-rules ()
    ((let-values-aux () ((formals init tmp) ...) body)
     (let ()
       (define-values tmp init)
       ...
       (let ()
	 (define-values formals (%apply values tmp))
	 ...
	 (let () . body))))
    ((let-values-aux ((formals init) . bindings) tmps body)
     (let-values-aux bindings ((formals init tmp) . tmps) body))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body1 body2 ...)
     (let () body1 body2 ...))
    ((let*-values ((formals init) . bindings) body1 body2 ...)
     (let-values ((formals init))
       (let*-values bindings body1 body2 ...)))
    ((let*-values . _)
     (syntax-error "bad let*-values syntax"))))
  
;;; Control features

(define (procedure? obj)
  (%procedure? obj))

(define (assert-procedure! obj)
  (unless (%procedure? obj)
    (error "not a procedure" obj)))

(define (apply proc arg . arg*)
  (assert-procedure! proc)
  (%apply proc (let loop ((arg arg) (arg* arg*))
		 (cond
		  ((null? arg*)
		   (assert-list! arg)
		   arg)
		  (else
		   (cons arg (loop (car arg*) (cdr arg*))))))))
  
(define (values . things)
  (%call-with-current-continuation
   (lambda (cont)
     (%apply cont things))))

(define (make-dynamic-point depth in out parent)
  (vector depth in out parent))
(define (dynamic-point-depth point)
  (%vector-ref point 0))
(define (dynamic-point-in point)
  (%vector-ref point 1))
(define (dynamic-point-out point)
  (%vector-ref point 2))
(define (dynamic-point-parent point)
  (%vector-ref point 3))

(define (get-dynamic-point)
  dynamic-point)
(define (set-dynamic-point! point)
  (set! dynamic-point point))

(define (travel-to-point! here target)
  (cond
   ((%eq? here target)
    (if #f #f))
   ((%fx< (dynamic-point-depth here) (dynamic-point-depth target))
    (travel-to-point! here (dynamic-point-parent target))
    ((dynamic-point-in target)))
   (else
    ((dynamic-point-out here))
    (travel-to-point! (dynamic-point-parent here) target))))

(define (dynamic-wind in body out)
  (in)
  (let ((here (get-dynamic-point)))
    (set-dynamic-point!
     (make-dynamic-point (%fx+ (dynamic-point-depth here) 1)
			 in
			 out
			 here))
    (let-values ((res* (body)))
      (set-dynamic-point! here)
      (out)      
      (%apply values res*))))

(define (call-with-current-continuation proc)
  (%call-with-current-continuation
   (lambda (cont)
     (proc (continuation->procedure cont (get-dynamic-point))))))

(define (continuation->procedure cont point)
  (lambda res*
    (travel-to-point! (get-dynamic-point) point)
    (set-dynamic-point! point)
    (apply cont res*)))

(define exit
  (case-lambda
   (() (exit #t))
   ((obj)
    (with-exception-handler
     (lambda (condition)
       (%exit obj))
     (lambda ()
       (travel-to-point! (get-dynamic-point) root)
       (%exit obj))))))

(define (list*? list*)
  (define (failure rest)
    (error "not a (circular) list" (car list*)))
  (let loop ((finite? #f) (list* list*))
    (if (null? list*)
	(and finite? #t)
	(loop (or (%length (car list*) failure) finite?)
	      (cdr list*)))))

(define (assert-list*! list*)
  (unless (list*? list*)
    (error "at least one list has to be finite" list*)))

(define (%map proc list)
  (let loop ((list list))
    (if (%null? list)
	'()
	(cons (proc (car list))
	      (loop (cdr list))))))

(define (any pred list)
  (let loop ((list list))
    (if (%null? list)
	#f
	(or (pred (car list))
	    (loop (cdr list))))))
	
(define (map proc . list*)
  (assert-procedure! proc)
  (assert-list*! list*)
  (let loop ((list* list*))
    (if (any null? list*)
	'()
	(cons
	 (%apply proc (%map car list*)) 
	 (loop (%map cdr list*))))))

(define (for-each proc . list*)
  (assert-procedure! proc)
  (assert-list*! list*)
  (let loop ((list* list*))
    (cond
     ((any null? list*)
      (if #f #f))
     (else
      (%apply proc (%map car list*))
      (loop (%map cdr list*))))))

;;; Parameter objects

(define make-parameter
  (case-lambda
   ((init)
    (make-parameter init (lambda (value) value)))
   ((init converter)
    (define value (converter init))
    (lambda args
      (cond
       ((null? args)
	value)
       ((%eq? (car args) <param-set!>)
	(set! value (cadr args)))
       ((%eq? (car args) <param-convert>)
	converter)
       (else
	(error "bad parameter syntax")))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((param value) ...) body1 body2 ...)
     (parameterize-aux () ((param value) ...) (body1 body2 ...)))
    ((parameterize . args)
     (syntax-error "bad parameterize syntax"))))

(define-syntax parameterize-aux
  (syntax-rules ()
    ((parameterize-aux
	 ((param value p old new) ...)
	 ()
	 body)
     (let ((p param) ...)
       (let ((old (p)) ...
	     (new ((p <param-convert>) value)) ...)
	 (dynamic-wind
	     (lambda () (p <param-set!> new) ...)
	     (lambda () . body)
	     (lambda () (p <param-set!> old) ...)))))
    ((parameterize-aux
      args
      ((param value) . rest)
      body)
     (parameterize-aux
      ((param value p old new) . args)
      rest
      body))))

;;; Equivalence predicates
(define (eq? obj1 obj2)
  (%eq? obj1 obj2))

;; FIXME: Number handling
(define (eqv? obj1 obj2)
  (%eq? obj1 obj2))

;;; Symbols

(define (symbol? obj)
  (%symbol? obj))

(define (assert-symbol! symbol)
  (unless (symbol? symbol)
    (error "not a symbol" symbol)))

(define (symbol->string symbol)
  (assert-symbol! symbol)
  (%symbol->string symbol))

;;; Booleans

(define (boolean? obj)
  (%boolean? obj))

(define (assert-boolean! obj)
  (unless (boolean? obj)
    (error "not a boolean" obj)))

(define (not obj)
  (if obj #f #t))

(define (boolean=? boolean*)
  (for-each assert-boolean! boolean*)
  (let loop ((boolean* boolean*))
    (or (null? boolean*)
	(null? (cdr boolean*))
	(and (%eq? (car boolean*) (cdr boolean*))
	     (loop (cdr boolean*))))))

;;; Lists

(define (null? obj)
  (%null? obj))

(define (cons car cdr)
  (%cons car cdr))

(define (pair? obj)
  (%pair? obj))

(define (car obj)
  (if (pair? obj)
      (%car obj)
      (error "not a pair" obj)))

(define (cdr obj)
  (if (pair? obj)
      (%cdr obj)
      (error "not a pair" obj)))

(define (list . list) list)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define %length
  (case-lambda
   ((obj)
    (%length obj (lambda (rest) #f)))
   ((obj failure)
    (let loop ((hare obj) (tortoise obj) (count 0))
      (if (pair? hare)
	  (let ((hare (cdr hare)))
	    (if (pair? hare)
		(let ((hare (cdr hare))
		      (tortoise (cdr tortoise)))
		  (and (not (%eq? hare tortoise))
		       (loop hare tortoise (%fx+ count 2))))
		(and (or (null? hare)
			 (begin (failure hare)
				#f))
		     (%fx+ count 1))))
	  (and (or (null? hare)
		   (begin (failure hare)
			  #f))
	       count))))))

(define (length list)
  (or (%length list)
      (error "not a list" list)))

(define (list? list)
  (and (%length list) #t))

(define (assert-list! obj)
  (unless (list? obj)
    (error "not a list")))

(define (reverse list)
  (assert-list! list)
  (let loop ((list list) (reversed '()))
    (if (%null? list)
	reversed
	(loop (cdr list) (cons (car list) reversed)))))

(define (memv obj list)
  (assert-list! list)
  (let loop ((list list))
    (and (not (null? list))
	 (if (eqv? (car list) obj)
	     list
	     (loop (cdr list))))))

;;; Characters

(define (char? obj)
  (%char? obj))

(define (assert-char! obj)
  (unless (char? obj)
    (error "not a char" obj)))

(define (char=? char*)
  (for-each assert-char! char*)
  (let loop ((char* char*))
    (or (null? char*)
	(null? (cdr char*))
	(and (%eq? (car char*) (cdr char*))
	     (loop (cdr char*))))))

;;; Strings

(define (string? obj)
  (%string? obj))

(define (string . char*)
  (for-each assert-char! char*)
  (%list->string char*))

(define (assert-string! obj)
  (unless (string? obj)
    (error "not a string" obj)))

(define (assert-string-index! k)
  (unless (and (%fixnum? k) (not (%fxnegative? k)))
    (error "invalid string index" k)))

(define string->list
  (case-lambda
   ((string)
    (assert-string! string)
    (%string->list string))
   ((string start)
    (assert-string! string)
    (assert-string-index! start)
    (%string->list string start))
   ((string start end)
    (assert-string! string)
    (assert-string-index! start)
    (assert-string-index! end)
    (%string->list string start end))))

(define (list->string list)
  (assert-list! list)
  (for-each assert-char! list)
  (%list->string list))

(define (string-for-each proc . string*)
  (assert-procedure! proc)
  (%apply
   for-each
   proc
   (map string->list string*)))

;;; Vectors

(define (assert-vector! vector)
  (unless (vector? vector)
    (error "not a vector" vector)))

(define (assert-vector-index! k)
  (unless (and (%fixnum? k) (not (%fxnegative? k)))
    (error "invalid vector index" k)))

(define (vector? obj)
  (%vector? obj))

(define make-vector
  (case-lambda
   ((k)
    (assert-vector-index! k)
    (%make-vector k))
   ((k fil)
    (assert-vector-index! k)
    (%make-vector k))))

(define (vector-length vector)
  (assert-vector! vector)
  (%vector-length vector))
    
(define (vector-ref vector k)
  (assert-vector! vector)
  (assert-vector-index! k)
  (%vector-ref vector k))

(define (vector-set! vector k obj)
  (assert-vector! vector)
  (assert-vector-index! k)
  (%vector-set! vector k obj))

(define (vector . element*)
  (list->vector element*))

(define (list->vector list)
  (let ((k (length list)))
    (let ((vector (%make-vector k)))
      (do ((i 0 (%fx+ i 1)) (list list (cdr list)))
	  ((%fx= i k) vector)
	(%vector-set! vector i (car list))))))

;;; Exceptions

(define (default-exception-handler condition)
  (%display condition) (%display "\n")
    ;; FIXME: Show condition on the console and exit with #f
    ;; display-error-object???
  (%exit #f))

(define current-exception-handlers
  (make-parameter (list default-exception-handler)))

(define (with-exception-handler handler thunk)
  (with-exception-handlers (cons handler (current-exception-handlers)) thunk))

(define (with-exception-handlers handlers thunk)
  (parameterize ((current-exception-handlers handlers))
    (thunk)))

(define (raise obj)
  (let ((handlers (current-exception-handlers)))
    (with-exception-handlers
     (%cdr handlers)
     (lambda ()
       ((%car handlers) obj)
       (error "exception not caught" obj)))))

(define (raise-continuable obj)
  (let ((handlers (current-exception-handlers)))
    (with-exception-handlers
     (cdr handlers)
     (lambda ()
       ((car handlers) obj)))))

(%set-exception-handler! raise)

(define (error message . irritant*)
  (%error message irritant*))

(define (append list1 list2)
  ;; TODO: More than one argument
  ;; FIXME: Check that first argument is a list
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;; Ports

(define-record-type <eof-object>
  (eof-object)
  eof-object?)

(define-record-type <input-port>
  (make-input-port read-char peek-char char-ready?)
  input-port?
  (read-char port-read-char)
  (peek-char port-peek-char)
  (char-ready? port-char-ready?))

(define-record-type <output-port>
  (make-output-port write-char type aux)
  output-port?
  (write-char port-write-char)
  (type output-port-type)
  (aux output-port-aux))

(define (make-system-output-port %output-port)
  (define (write-char char)
    (%write-char char %output-port))
  (make-output-port write-char 'system-port #f))

(define (assert-output-port! port)
  (unless (output-port? port)
    (error "not a textual output port" port)))

(define (textual-port? obj)
  (or (input-port? obj) (output-port? obj)))

(define write-char
  (case-lambda
   ((char)
    (write-char char (current-output-port)))
   ((char port)
    (assert-char! char)
    (assert-output-port! port)
    ((port-write-char port) char))))

(define write-string
  (case-lambda
   ((string)
    (write-string string (current-output-port))) 
   ((string port)
    (assert-string! string)
    (assert-output-port! port)
    (let ((write-char (port-write-char port)))
      (string-for-each write-char string)))))

(define (open-output-string)
  (define %char* '())
  (define (write-char char)
    (set! %char* (cons char %char*)))
  (define (get-output-string)
    (list->string (reverse %char*)))
  (make-output-port write-char 'string-port get-output-string))

(define (get-output-string port)
  (unless (and (output-port? port) (eq? (output-port-type port) 'string-port))
    (error "port was not created by ‘open-output-string’" port))
  ((output-port-aux port)))

;;; Output

(define (write . args)
  ;; FIXME
  (error "not implemented"))

(define display
  (case-lambda
   ((obj)
    (display obj (current-output-port)))
   ((obj port)
    (assert-output-port! port)
    (cond
     ((string? obj)
      (write-string obj port))
     ((char? obj)
      (write-char obj port))
     ((symbol? obj)
      ;; FIXME: How to display |...|-symbols?
      (write-string (%symbol->string obj)))
     (else
      ;; FIXME
      (write-string "<unspecified>"))))))

(define newline
  (case-lambda
   (()
    (newline (current-output-port)))
   ((port)
    (assert-output-port! port)
    ;; FIXME: The line ending depends on the host system
    (write-char #\newline port))))

;;; Quasiquotation

(define-syntax unquote
  (syntax-rules ()
    ((unquote . args)
     (syntax-error "invalid use of auxiliary syntax ‘unquote’"))))

(define-syntax unquote-splicing
  (syntax-rules ()
    ((unquote-splicing . args)
     (syntax-error "invalid use of auxiliary syntax ‘unquote-splicing’"))))

(define-syntax quasiquote
  (syntax-rules ()
    ((quasiquote template)
     (quasiquote-aux template))
    ((quasiquote . args)
     (syntax-error "bad quasiquotation"))))

(define-syntax quasiquote-aux
  (syntax-rules (quasiquote unquote unquote-splicing)
    ((quasiquote-aux ,form)
     form)   
    ((quasiquote-aux (,@form . rest))
     (append form (quasiquote rest)))
    ((quasiquote-aux `form . depth)
     (list 'quasiquote (quasiquote-aux form #f . depth)))
    ((quasiquote-aux ,form #f . depth)
     (list 'unquote (quasiquote-aux form . depth)))
    ((quasiquote-aux ,@form x . depth)
     (list 'unquote-splicing (quasiquote-aux form . depth)))
    ((quasiquote-aux (car . cdr) . depth)
     (cons (quasiquote-aux car . depth) (quasiquote-aux cdr . depth)))
    ((quasiquote-aux #(element ...) . depth)
     (list->vector (quasiquote-aux (element ...) . depth)))
    ((quasiquote-aux constant . depth)
     'constant)))

;;; Formatting

(define (format format-string . objects)
  (let ((buffer (open-output-string)))
    (let loop ((format-list (string->list format-string))
	       (objects objects))
      (cond
       ((null? format-list) (get-output-string buffer))
       ((char=? (car format-list) #\~)
	(if (null? (cdr format-list))
	    (error "format: incomplete escape sequence" format-string)
	    (case (cadr format-list)
	      ((#\a)
	       (cond
		((null? objects)
		 (error "format: no value for escape sequence ‘~a’" format-string))
		(else
		 (display (car objects) buffer)
		 (loop (cddr format-list) (cdr objects)))))
	      ((#\s)
	       (cond
		((null? objects)
		 (error "format: no value for escape sequence ‘~s’" format-string))
		(else
		 (write (car objects) buffer)
		 (loop (cddr format-list) (cdr objects)))))
	      ((#\%)
	       (newline buffer)
	       (loop (cddr format-list) objects))
	      ((#\~)
	       (write-char #\~ buffer)
	       (loop (cddr format-list) objects))
	      (else
	       (error (format "format: unrecognized escape sequence ‘~~~a’"
			      (cadr format-list)) format-string)))))
       (else
	(write-char (car format-list) buffer)
	(loop (cdr format-list) objects))))))

;;; Constants

(define root (make-dynamic-point 0 #f #f #f))
(define dynamic-point root)

(define <param-set!> (vector #f))
(define <param-convert> (vector #f))

(define current-output-port (make-parameter (make-system-output-port (%current-output-port))))
