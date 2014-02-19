(define-library (rapid asmjs2)
  (export compile-module)
  (import (scheme base) (scheme case-lambda)
    (rapid asmjs))
  (begin
    
    ;
    ; Module is:
    ;
    ; (module Rap
    ;   (var x 0) 
    ;
    ;   (function signed alloc (()) body)
    ;
    ;
    
    (define make-global-var
      (make-parameter (lambda (i)
        (string-append "$g" (number->string i)))))
     
    (define make-local-var
      (make-parameter (lambda (i)
        (string-append "$l" (number->string i)))))
        
    (define (global-var i)
      ((make-global-var) i))
      
    (define (local-var i)
      ((make-local-var) i))
    
    (define (make-variable id type)
      (vector id type))
      
    (define (variable-id var)
      (vector-ref var 0))
      
    (define (variable-type var)
      (vector-ref var 1))
    
    (define (number-type number)
      (if (exact? number) 'int 'double))
    
    (define (with-globals mod k)
      (let loop ((global-count 0) (globals '()) (decl* (list-tail mod 2)))
        (if (null? decl*)
          (k globals)
          (let ((decl (car decl*)))
            (case (car decl)
              ((var)
                (loop (+ global-count 1)
                  (cons (cons (list-ref decl 1)
                      (make-variable (global-var global-count) (number-type (list-ref decl 2))))
                    globals) (cdr decl*)))
              ((function)
                (loop (+ global-count 1)
                  (cons (cons (list-ref decl 2)
                      (make-variable (global-var global-count)
                        (list (list (list-ref decl 1) (map (lambda (param) (list-ref param 0)) (list-ref decl 3))))))
                    globals) (cdr decl*)))
              ((stdlib)
                (loop (+ global-count 1)
                  (cons (cons (list-ref decl 1)
                      (make-variable (global-var global-count) (stdlib-type (list-ref decl 2))))
                    globals) (cdr decl*)))
              ((heap)
                (loop (+ global-count 1)
                  (cons (cons (list-ref decl 1)
                      (make-variable (global-var global-count) (heap-type (list-ref decl 2))))
                    globals) (cdr decl*)))
              ((foreign)
                (loop (+ global-count 1)
                  (cons (cons (list-ref decl 2)
                      (make-variable (global-var global-count) (list-ref decl 1)))
                    globals) (cdr decl*)))
              ((math)
                (loop (+ global-count 1)
                  (cons (cons (list-ref decl 1)
                      (make-variable (global-var global-count) (math-type (list-ref decl 2))))
                    globals) (cdr decl*)))                    
              (else (loop global-count globals (cdr decl*))))))))
                
    (define (stdlib-type s)
      (cdr (assoc s *stdlib-types*)))
      
    (define *stdlib-types*
      '(("Infinity" . double) ("NaN" . double)))

    (define (heap-type s)
      (cdr (assoc s *heap-types*)))

    (define *heap-types*
      '(("Uint8Array" . (1 intish))
        ("Int8Array" . (1 intish))
        ("Uint16Array" . (2 intish))
        ("Int16Array" . (2 intish))
        ("Uint32Array" . (4 intish))
        ("Int32Array" . (4 intish))
        ("Float32Array" . (4 doublish))
        ("Float64Array" . (8 doublish))))

    (define (math-type s)
      (cdr (assoc s *math-types*)))
      
    (define *math-types*
      `(("acos" . ((double (doublish))))
        ("asin" . ((double (doublish))))
        ("atab" . ((double (doublish))))
        ("cos" . ((double (doublish))))
        ("sin" . ((double (doublish))))
        ("tan" . ((double (doublish))))
        ("ceil" . ((double (doublish))))
        ("floor" . ((double (doublish))))
        ("exp" . ((double (doublish))))
        ("log" . ((double (doublish))))
        ("sqrt" . ((double (doublish))))
        ("abs" . ((signed (signed)) (double (doublish))))
        ("atan2" . ((double (doublish doublish))))
        ("pow" . ((double (doublish doublish))))
        ("imul" . ((signed (int int))))
        ("E" . double)
        ("LN10" . double)
        ("LN2" . double)
        ("LOG2E" . double)
        ("LOG10E" . double)
        ("PI" . double)
        ("SQRT1_2" . double)
        ("SQRT2" . double)))

    (define (compile-module mod)
      (with-globals mod
        (lambda (globals)
          `(function (id ,(list-ref mod 1)) ((id "$s") (id "$f") (id "$h"))
            (statement (string "use asm")) .
            ,(let loop ((decl* (list-tail mod 2)))
              (if (null? decl*)
                '()
                (let ((decl (car decl*)))
                  (case (car decl)
                    ((var)
                      `((var (id ,(variable-id (find-variable (list-ref decl 1) globals)))
                          (number ,(list-ref decl 2))) . ,(loop (cdr decl*))))
                    ((stdlib)
                      `((var (id ,(variable-id (find-variable (list-ref decl 1) globals)))
                          (member (id "$s") (id ,(list-ref decl 2)))) . ,(loop (cdr decl*))))
                    ((math)
                      `((var (id ,(variable-id (find-variable (list-ref decl 1) globals)))
                          (member (member (id "$s") (id "Math")) (id ,(list-ref decl 2)))) . ,(loop (cdr decl*))))
                    ((heap)
                      `((var (id ,(variable-id (find-variable (list-ref decl 1) globals)))
                          (new (member (id "$s") (id ,(list-ref decl 2))) (id "$h"))) . ,(loop (cdr decl*))))
                    ((foreign)
                      `((var (id ,(variable-id (find-variable (list-ref decl 2) globals)))
                          ,(coerce 'function (list-ref decl 1) `(member (id "$f") (id ,(list-ref decl 3))))) . ,(loop (cdr decl*)))) 
                    ((return)
                      (if (pair? (list-ref decl 1))
                        `((return (object ,@(map (lambda (property)
                                  `((id ,(car property)) . (id ,(variable-id (find-variable (cdr property) globals)))))
                                (list-ref decl 1))) . ,(loop (cdr decl*))))
                        `((return (id ,(variable-id (find-variable (list-ref decl 1) globals)))) . ,(loop (cdr decl*)))))
                    ((function)
                      (let ((param* (list-ref decl 3)))
                        (let-values (((local-count locals)
                              (let loop ((i 0) (locals '()) (param* param*))
                                (if (null? param*)
                                  (values i locals)
                                  (let ((param (car param*)))
                                    (loop (+ i 1) (cons (cons (list-ref param 1) (make-variable (local-var i) (list-ref param 0))) locals) (cdr param*)))))))
                           `((function (id ,(variable-id (find-variable (list-ref decl 2) globals)))
                            ,(map (lambda (param) `(id ,(variable-id (find-variable (list-ref param 1) globals locals)))) param*)
                            ,@(map (lambda (param)
                                (let ((var (find-variable (list-ref param 1) globals locals)))
                                  `(assignment (id ,(variable-id var)) ,(coerce 'function (variable-type var) `(id ,(variable-id var)))))) param*)
                            ,@(compile-body (list-ref decl 1) (list-tail decl 4) globals local-count locals)) . ,(loop (cdr decl*))))))
                    (else (error)))
          )))))))

    ; IDEA: environment becomes a procedure with dispatch
    (define (compile-body return-type stmt* globals local-count locals)
      (let loop ((local-count local-count) (locals locals) (stmt* stmt*))
        (if (null? stmt*)
          '()
          (let ((stmt (car stmt*)))
            (case (car stmt)
              ((var)
                (let ((var (make-variable (local-var local-count) (number-type (list-ref stmt 2)))))
                  `((var (id ,(variable-id var)) (number ,(list-ref stmt 2)))
                    . ,(loop (+ local-count 1) (cons (cons (list-ref stmt 1) var) locals) (cdr stmt*)))))
              ; special case for labelled statement? (good idea...) but ... own namespace
              ((return)
                (cons
                  (if (and (null? (cdr stmt*)) (> (length stmt) 1))
                    (let ((arg (compile-expression 'function (list-ref stmt 1) globals locals)))
                      (cond
                        ((eq? arg 'number)
                          `(return ,arg))
                        ((eq? return-type 'signed)
                          `(return (binary "|" ,arg (number 0))))
                        ((eq? return-type 'double)
                          `(return (unary "+" ,arg)))
                        (else
                          `(return ,arg)))) 
                    (compile-statement return-type stmt globals locals))
                  (loop local-count locals (cdr stmt*))))
              (else
                (cons (compile-statement return-type stmt globals locals) (loop local-count locals (cdr stmt*)))))))))

    (define (compile-statement return-type stmt globals locals)
      (if (pair? stmt)      
        (case (car stmt)
          ((return)
            (if (> (length stmt) 1)
              `(return ,(compile-expression return-type (list-ref stmt 1) globals locals))
              '(return)))
          ((begin) ; how do we call "begin" for comma-expressions -> begin? 
            `(block . ,(compile-statement* return-type (cdr stmt) globals locals)))
          ((if)
             `(if ,(compile-expression 'int (list-ref stmt 1) globals locals)
              ,(compile-statement return-type (list-ref stmt 2) globals locals) .
              ,(compile-statement* return-type (list-tail stmt 3) globals locals)))
          ((while)
            `(while ,(compile-expression 'int (list-ref stmt 1) globals locals)
              ,(compile-statement return-type (list-ref stmt 2) globals locals)))
          ((do)
            `(do ,(compile-expression 'int (list-ref stmt 1) globals locals)
              ,(compile-statement return-type (list-ref stmt 2) globals locals)))
          ((for)
            `(for ,(compile-expression* 'function (list-ref stmt 1) globals locals)
              ,(compile-expression* 'int (list-ref stmt 2) globals locals)
              ,(compile-expression* 'function (list-ref stmt 3) globals locals)
              ,(compile-statement return-type (list-ref stmt 4) globals locals)))
          (else `(statement ,(compile-expression 'void stmt globals locals))))
        `(statement ,(compile-expression 'void stmt globals locals))))

    (define (compile-statement* return-type stmt* globals locals)
      (map (lambda (stmt) (compile-statement return-type stmt globals locals)) stmt*))

    (define (compile-expression* expected-type expr* globals locals)
      (map (lambda (expr) (compile-expression expected-type expr globals locals)) expr*))

    (define (compile-expression expected-type expr globals locals)
      (cond
        ((number? expr)
          (coerce
            (cond
              ((inexact? expr) 'double)
              ((negative? expr) 'signed)
              ((< expr #x8000000) 'fixnum)
              (else 'int)) expected-type `(number ,expr)))
        ((symbol? expr)
          (let ((var (find-variable expr globals locals)))
            (coerce (variable-type var) expected-type `(id ,(variable-id var)))))
        (else ('x expr))))
        
    (define find-variable
      (case-lambda
        ((var globals) (find-variable var globals '()))
        ((var globals locals) (cdr (or (assq var locals) (assq var globals))))))

    (define (coerce from to js)
      (case to
        ((int)
          (case from
            ((fixnum signed unsigned) js)
            ((double) `(unary "~" (unary "~" ,js)))
            (else `(binary "|" ,js (number 0)))))
        ((signed)
          (case from
            ((double) `(unary "~" (unary "~" ,js)))
            ((fixnum signed) js)
            (else `(binary "|" ,js (number 0)))))
        ((double)
          (case from
            ((double) js)
            (else `(unary "+" ,js))))
        ((void function) js)
        (else (error from to js))))

    (emit
      (compile-module
      `(module "RapidModule"
        (var x 0)
        (var y 0.0)
        (stdlib NaN "NaN")
        (math abs "abs")
        (heap hu32 "Uint32Array")
        (foreign function call-error "callError")
        (foreign int heap-size "heapSize")
        (function signed alloc ((int m))
          (var z 0)
          (if 0.3 z z)
          (return z))
        (return (("a" . x)))
    )))
    
    ))
