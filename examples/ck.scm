#;(import (scheme base)
	(scheme case-lambda)
	(scheme write))

(import (rapid primitive))

(define-syntax aux (syntax-rules ... ()))

;; use ck-aux

(define-syntax ck
  (syntax-rules ... (quote)
    ((ck () 'v) v)
    ((ck (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))
    ((ck s "arg" (op va ...))
     (op s va ...))
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))
    ((ck s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))
    ((ck s (op ea ...))
     (ck s "arg" (op) ea ...))))

(define-syntax ck*
  (syntax-rules ... (quote)
    ((ck* () 'v) v)
    ((ck* (((op ...) ea ...) . s) 'v)
     (ck* s "arg" (op ... 'v) ea ...))
    ((ck* s "arg" (op va ...))
     (op aux s va ...))
    ((ck* s "arg" (op ...) 'v ea1 ...)
     (ck* s "arg" (op ... 'v) ea1 ...))
    ((ck* s "arg" (op ...) ea ea1 ...)
     (ck* (((op ...) ea1 ...) . s) ea))
    ((ck* s (op ea ...))
     (ck* s "arg" (op) ea ...))))

;; TODO: Rename a few things
(define-syntax define-macro
  (syntax-rules ... (case-lambda)
    ((define-macro op ellipsis (literal ...)
       (case-lambda 
	(%formals %expression)
	...))
     (begin
       (define-syntax define-macro-aux
	 (syntax-rules ::: (ellipsis quote)
	   ((define-macro-aux ((pattern expression) :::) ())
	    (define-syntax op
	      (syntax-rules ellipsis (literal ... quote aux)
		((op aux s . pattern) (ck* s expression)) :::
		((op . args) (ck* () (op . args))))))
	   ((define-macro-aux pt* ((formals expression) . fe*))
	    (define-macro-aux pt* fe* (formals expression) ()))
	   ((define-macro-aux (pt :::) fe* (() expression) formals)
	    (define-macro-aux (pt ::: (formals expression)) fe*))
	   ((define-macro-aux pt* fe* ((x ellipsis . formals) expression)
	      (f :::))
	    (define-macro-aux pt* fe* (formals expression)
	      (f ::: 'x ellipsis)))
	   ((define-macro-aux pt* fe* ((x . formals) expression)
	      (f :::))
	    (define-macro-aux pt* fe* (formals expression)
	      (f ::: 'x)))))     
       (define-macro-aux () ((%formals %expression) ...))))))

(define-macro $cons ... ()
  (case-lambda
   ((h t) '(h . t))))

(define-macro $append ... ()
  (case-lambda
   ((() l2) 'l2)
   (((h . t) l2) ($cons 'h ($append 't 'l2)))))

(define-macro $quote ... ()
  (case-lambda
   ((x) ''x)))

;; The macro below won't work that simply...
;; because it has to thread everything to kt and kf
;; need macros that expand in macros, not values... (ok!)
(define-macro $eq? ... ()
  (case-lambda
   ((x y kt kf)  ;; problem: have to be quoted
    '(let-syntax ((test
		   (syntax-rules (... ...) (x)
		     ((test x) kt)
		     ((test _) kf))))
       (test y)))))

(display ($quote ($append '(1 2 3) '(4 5))))
(newline)
