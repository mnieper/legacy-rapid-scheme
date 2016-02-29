;; Implement Meta-Macros/Special forms like c-if...
;;   for the high-level macro system
;; Special forms don't have their arguments expanded
;;   but need a later call


(import (scheme base)
	(scheme write))
;(import (rapid primitive))

(define-syntax :call (syntax-rules ... ()))
(define-syntax :prepare (syntax-rules ... ()))

(define-syntax ck
  (syntax-rules ... (quote)
    ((ck () 'v) v)
    ((ck (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))
    ((ck s "arg" (op va ...))
     (op :call s va ...))
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))
    ((ck s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))
    ((ck s (op ea ...))
     (op :prepare s ea ...))))

(define-syntax define-macro
  (syntax-rules ... ()
    ((define-macro op ellipsis (literal ...)
       (pattern template)
       ...)
     (begin
       (define-syntax d
	 (syntax-rules ...1 (ellipsis op quote)
	   ((d o e (l ...1) () ((p t) ...1))
	    (define-syntax o
	      (syntax-rules e (l ...1 quote aux)
		((o :call s . p) (ck s t))
		...1
		((o :prepare s ea ...1) (ck s "arg" (o) ea ...1))
		((o . args) (ck () (o . args))))))
	   ((d o e l* (((op . p) t) . pt*) qu*)
	    (d o e l* pt* qu* (p t) ()))
	   ((d o e l* pt* (qu ...1) (() t) p)
	    (d o e l* pt* (qu ...1 (p t))))
	   ((d o e l* pt* qu* ((x . p) t) (y ...1))
	    (d o e l* pt* qu* (p t) (y ...1 x)))))
       (d op ellipsis (literal ...) ((pattern template) ...) ())))))

;; TODO: Rename a few things
;; FIXME: Need to pass a couple of things to the inner macro
;;        Chibi does it right; don't test with gosh
#;(define-syntax define-macro
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

(define-macro m-cons ... ()
  ((m-cons 'h 't) '(h . t)))

(define-macro m-append ... ()
  ((m-append '() 'l2) 'l2)
  ((m-append '(h . t) 'l2) (m-cons 'h (m-append 't 'l2))))

(define-macro m-quote ... ()
  ((m-quote 'x) ''x))

;; The macro below won't work that simply...
;; because it has to thread everything to kt and kf
;; need macros that expand in macros, not values... (ok!)
#;(define-macro $eq? ... ()
  (case-lambda
   ((x y kt kf)  ;; problem: have to be quoted
    '(let-syntax ((test
		   (syntax-rules (... ...) (x)
		     ((test x) kt)
		     ((test _) kf))))
       (test y)))))

; besides if, we want (gensym) on the macro level!

(display (m-quote (m-append '(1 2 3) '(4 5))))
(newline)
