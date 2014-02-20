;
; Types
; -----
;
; From:
;
; 
;
;
;
; To:
;
; void - do not coerce at all
; extern (= signed or double) - for external function calls
; signed - for switch, return
; double - for return
; doublish - for *
; int - for loops
; intish - for |, &, ..., heap index
; signed+unsigned+doublish - for /, %
; signed+unsigned+double - for <, <=, ...
;
; From:
; int, intish, signed, double, unsigned, doublish
;

; 
; Problem: f(-x) : -x als extern; d.h. x als signed oder double???
; Ist x int, so brauchen wir -x|0; Ist x doublish, so reicht -x...
; Was ändert dies am Ergebnis? fast nix... also extern für - wird doublish.
;
;
; Problem: (a - b) < (c - d) : Wir brauchen: a - b als signed+unsigned+double
; Wie? a-b als Double ergibt keinen Sinn; a, b als Int... schon; Terme also typisiert?
;
;
;
;

(define-library (rapid module)
  (export compile-module make-global-var make-local-var make-label)
  (import (scheme base) (scheme case-lambda) (rapid asmjs))
  (begin

    (define-syntax new-environment
      (syntax-rules ()
        ((new-environment . body)
          (parameterize
            ((environment (make-environment 0 '() (make-parameter #f)))) .
              body))))

    (define-syntax new-frame
      (syntax-rules ()
        ((new-frame . body)
          (parameterize
            (((environment-frame (environment)) (make-frame 0 '() 0 '()))) . body))))

    (define (compile-module mod)
      (new-environment
        (for-each globals-update! (list-tail mod 2))
        `(function (id ,(list-ref mod 1)) ((id "$s") (id "$f") (id "$h"))
          (statement (string "use asm")) .
          ,(map compile-decl (list-tail mod 2)))))

    (define (globals-update! decl)
      (case (car decl)
        ((signed unsigned double)
          (global-set! (list-ref decl 1) (list-ref decl 0)))
        ((stdlib)
          (global-set! (list-ref decl 1) (stdlib-type (list-ref decl 2))))
        ((math)
          (global-set! (list-ref decl 1) (math-type (list-ref decl 2))))
        ((heap)
          (global-set! (list-ref decl 1) (heap-type (list-ref decl 2))))
        ((foreign)
          (global-set! (list-ref decl 2) (list-ref decl 1)))
        ((function)
          (global-set! (list-ref decl 2)
            (list
              (list (list-ref decl 1)
                (map (lambda (param) (list-ref param 0))
                  (list-ref decl 3))))))))
    
    (define (compile-decl decl)
      (case (car decl)
        ((signed unsigned double)
          `(var ,(id (list-ref decl 1)) (apply initializer (list-ref decl 0)
              (list-tail decl 2))))
        ((stdlib)
          `(var ,(id (list-ref decl 1))
            (member (id "$s") ,(list-ref decl 2))))
        ((math)
          `(var ,(id (list-ref decl 1))
            (member (member (id "$s") "Math") ,(list-ref decl 2))))
        ((heap)
          `(var ,(id (list-ref decl 1))
            (new (member (id "$s") ,(list-ref decl 2)) (id "$h"))))
        ((foreign)
          `(var ,(id (list-ref decl 2))
            ,(coerce 'function (list-ref decl 1)
              `(member (id "$f") ,(list-ref decl 3)))))
        ((function)
          (new-frame
            (for-each (lambda (param)
                (local-set! (list-ref param 1) (list-ref param 0)))
              (list-ref decl 3))
            `(function ,(id (list-ref decl 2))
              ,(map (lambda (param) (id (list-ref param 1))) (list-ref decl 3))
              ,@(map (lambda (param)
                  (let ((val (value (list-ref param 1))))
                    `(assignment ,(value-id val)
                      ,(coerce 'function (value-type val) (value-id val)))))
                (list-ref decl 3))
              ,@(compile-body (list-ref decl 1) (list-tail decl 4))))) 
        ((return)
          `(return
            ,(if (pair? (list-ref decl 1))
              `(object
                ,@(map (lambda (property)
                    `((id ,(car property)) . ,(id (cdr property))))
                  (list-ref decl 1)))
               (id (list-ref decl 1)))))
        (else    
          (error compile-decl "illegal declaration" decl))))

    (define (compile-body return-type stmt*)
      (if (null? stmt*)
        '()
        (let ((js
              (let ((stmt (car stmt*)))
                (if (pair? stmt)
                  (case (car stmt)
                    ((signed unsigned double)
                      (local-set! (list-ref stmt 1) (list-ref stmt 0))
                      `(var ,(id (list-ref stmt 1))
                        (apply initializer (list-ref stmt 0) (list-tail stmt 2))))
                    ((return)
                      (if (and (null? (cdr stmt*)) (> (length stmt) 1))
                        (let ((arg (compile-expression 'function (list-ref stmt 1))))
                          (if (eq? (car arg) 'number)
                            `(return ,arg)
                            (case return-type
                              ((signed unsigned) 
                                `(return (binary "|" ,arg (number 0))))
                              ((double)
                                `(return (unary "+" ,arg)))
                              (else (error compile-body "unknown return type" return-type)))))
                        (compile-statement return-type stmt)))                        
                    (else
                      (compile-statement return-type stmt)))
                  (compile-statement return-type stmt)))))
          (cons js (compile-body return-type (cdr stmt*))))))

    (define (compile-statement return-type stmt)
      (if (pair? stmt)
        (case (car stmt)
          ((return)
            `(return .
              ,(if (> (length stmt) 1)
                `(,(compile-expression return-type (list-ref stmt 1)))
                '())))
          ((begin)
            `(block . ,(compile-statement* return-type (cdr stmt))))
          ((if)
            `(if ,(compile-expression 'int (list-ref stmt 1))
              ,(compile-statement return-type (list-ref stmt 2))
              ,(compile-statement* return-type (list-tail stmt 3))))
          ((while)
            `(while ,(compile-expression 'int (list-ref stmt 1))
              ,(compile-block return-type (list-tail stmt 2))))
          ((do)
            `(do ,(compile-expression 'int (list-ref stmt 1))
              ,(compile-block return-type (list-tail stmt 2))))
          ((for)
            `(for ,(compile-expression* 'function (list-ref stmt 1))
              ,(compile-expression* 'int (list-ref stmt 2))
              ,(compile-expression* 'function (list-ref stmt 3))
              ,(compile-block return-type (list-tail stmt 4))))
          ((break continue)
            `(,(list-ref stmt 0) .
              ,(if (> (length stmt) 1)
                `(,(label-id (list-ref stmt 1)))
                '())))
          ((label)
            `(labelled ,(label-id (list-ref stmt 1))
              ,(compile-statement return-type (list-ref stmt 2))))
          ((switch)
            `(switch ,(compile-expression 'signed (list-ref stmt 1)) .
              ,(compile-statement* return-type (list-tail stmt 2))))
          ((case)
            `(case ,(compile-expression 'signed (list-ref stmt 1)) .
              ,(compile-statement* return-type (list-tail stmt 2))))
          ((default)
            `(default . ,(compile-statement* return-type (list-tail stmt 1))))          
          (else `(statement ,(compile-expression 'void stmt))))
        `(statement ,(compile-expression 'void stmt))))
    
    (define (compile-block return-type stmt*)
      (if (null? (cdr stmt*))
        (compile-statement return-type (car stmt*))
        `(block ,@(compile-statement* return-type stmt*)))) 
    
    (define (compile-statement* return-type stmt*)
      (map (lambda (stmt) (compile-statement return-type stmt)) stmt*))

    (define (compile-expression expected-type expr)
      (cond
        ((number? expr)
          (coerce
            (cond
              ((inexact? expr) 'double)
              ((negative? expr) 'signed)
              ((< expr #x80000000) 'fixnum)
              (else 'int)) expected-type `(number ,expr)))
        ((symbol? expr)
          (let ((val (value expr)))
            (coerce (value-type val) expected-type (value-id val))))
        (else
          (case (car expr)
            ((begin)
              `(expression .
                ,(let loop ((expr* (cdr expr)))
                  (if (null? (cdr expr*))
                    `(,(compile-expression expected-type (car expr*)))
                    `(,(compile-expression 'void (car expr*)) .
                      ,(loop (cdr expr*)))))))
            ((ref)
              (let ((val (value (list-ref expr 1))))
                (let ((type (value-type val)) (id (value-id val)))              
                  (cond
                    ((number? (list-ref expr 2))
                      (coerce (list-ref type 1) expected-type
                        `(member (id "$h")
                          (number ,(/ (list-ref expr 2) (list-ref type 0))))))
                    ((= (list-ref type 0) 1)
                      (coerce 'intish expected-type
                        `(member (id "$h")
                           ,(compile-expression 'int (list-ref expr 2)))))
                    (else
                      (coerce (list-ref type 1) expected-type
                        `(member (id "$h")
                          (binary ">>"
                            ,(compile-expression 'intish (list-ref expr 2))
                            (number ,(list-ref type 2))))))))))
            ((set!)
              `(assignment ,(compile-expression 'function (list-ref expr 1))
                ,(compile-expression expected-type (list-ref expr 2))))
            ((double signed unsigned int)
              (coerce (list-ref expr 0) expected-type
                (compile-expression (list-ref expr 0) (list-ref expr 1))))            
            ((truncate)
              (coerce 'signed expected-type
                `(unary "~"
                  (unary "~" ,(compile-expression 'double (list-ref expr 1))))))
            ; XXX +cexpr
            ((+ -) ; TODO special case +; special case *
              (if (> 2 (length expr))
                (compile-binary expected-type expr)
                (compile-unary expected-type expr)))
            ((~ not)
              (compile-unary expected-type expr))
            ((* / remainder << >> >>> < > <= >= = not= and ^ or)
              (compile-binary expected-type expr))
            ((if)
              (let ((type
                    (case expected-type
                      ((double doublish) 'double)
                      (else 'int))))
                (coerce type expected-type
                  `(conditional
                    ,(compile-expression 'int (list-ref expr 1))
                    ,(compile-expression type (list-ref expr 2))
                    ,(compile-expression type (list-ref expr 3))))))
            (else (error compile-expression "illegal expression" expr))))))

    (define (compile-binary expected-type expr)
      (error "not yet implemented")
      )
      
    (define (compile-unary expected-type expr)
      (let ((op (unary-op (list-ref expr 0))))
        (let-values (((from to) ((op-type op) expected-type)))
          (coerce to expected-type
            `(unary ,(op-name op)
              ,(compile-expression from (list-ref expr 1)))))))

    (define (compile-expression* expected-type expr*)
      (map (lambda (expr) (compile-expression expected-type expr)) expr*))

    (define (coerce from to js)
      (case to
        ((signed+unsigned+doublish)
          (case from
            ((int intish) `(binary "|" ,js (number 0)))
            (else js)))
        ((signed+unsigned+double)
          (case from
            ((int intish) `(binary "|" ,js (number 0)))
            ((doublish) `(unary "+" ,js))
            (else js)))
        ((intish)
          (case from
            ((double doublish) `(unary "~" (unary "~" ,js)))
            (else js)))
        ((int)
          (case from
            ((fixnum signed unsigned int) js)
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
        ((void function) js) ; TODO Remove function type
        (else (error from to js))))
    
    (define-record-type <value>
      (make-value id type)
      value?
      (id value-id)
      (type value-type))
    
    (define-record-type <environment>
      (make-environment global-count globals frame)
      environment?
      (global-count environment-global-count environment-global-count-set!)
      (globals environment-globals environment-globals-set!)
      (frame environment-frame))

    (define-record-type <frame>
      (make-frame local-count locals label-count labels)
      frame?
      (local-count frame-local-count frame-local-count-set!)
      (locals frame-locals frame-locals-set!)
      (label-count frame-label-count frame-label-count-set!)
      (labels frame-labels frame-labels-set!))
      
    (define environment
      (make-parameter #f))

    (define (frame)
      ((environment-frame (environment))))

    (define (global-set! global type)
      (let ((count (environment-global-count (environment))))
        (environment-globals-set! (environment)
          (cons
            (cons global (make-value `(id ,(global-var count)) type))
            (environment-globals (environment))))
        (environment-global-count-set! (environment) (+ count 1))))
    
    (define (local-set! local type)
      (let ((count (frame-local-count (frame))))
        (frame-locals-set! (frame)
          (cons
            (cons local (make-value `(id ,(local-var count)) type))
            (frame-locals (frame))))
        (frame-local-count-set! (frame) (+ count 1))))
    
    (define (label-set! name)
      (let ((count (frame-label-count (frame))))
        (frame-labels-set! (frame)
          (cons
            (cons name `(id ,(label count)))
            (frame-labels (frame))))
        (frame-label-count-set! (frame) (+ count 1))))
    
    (define (value name)
      (cdr
        (or
          (and (frame) (assq name (frame-locals (frame))))
          (assq name (environment-globals (environment)))
          (error value "variable not defined" name))))
          
    (define (id name)
      (value-id (value name)))
      
    (define (type name)
      (value-type (value name)))

    (define (label-id name)
      (cdr
        (or (assq name (frame-labels (frame)))
          (begin
            (label-set! name)
            (assq name (frame-labels (frame)))))))

    (define make-global-var
      (make-parameter (lambda (i)
        (string-append "$g" (number->string i)))))

    (define make-local-var
      (make-parameter (lambda (i)
        (string-append "$v" (number->string i)))))
    
    (define make-label
      (make-parameter (lambda (i)
        (string-append "$l" (number->string i)))))
    
    (define (global-var i)
      ((make-global-var) i))
      
    (define (local-var i)
      ((make-local-var) i))

    (define (label i)
      ((make-label) i))

    (define initializer
      (case-lambda
        ((type) (initializer type 0))
        ((type init)
          `(number
            ,(case type
              ((unsigned signed) (exact init))
              ((double) (inexact init))
              (else (error initializer "illegal type" type)))))))
      
    (define (number-type number)
      (if (exact? number) 'int 'double))
      
    (define (stdlib-type s)
      (cdr (assoc s *stdlib-types*)))
      
    (define *stdlib-types*
      '(("Infinity" . double) ("NaN" . double)))

    (define (heap-type s)
      (cdr (assoc s *heap-types*)))

    (define *heap-types*
      '(("Uint8Array" . (1 intish 0))
        ("Int8Array" . (1 intish 0))
        ("Uint16Array" . (2 intish 1))
        ("Int16Array" . (2 intish 1))
        ("Uint32Array" . (4 intish 2))
        ("Int32Array" . (4 intish 2))
        ("Float32Array" . (4 doublish 2))
        ("Float64Array" . (8 doublish 3))))

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

    (define (unary-op s)
      (cdr (assq s *unary-ops*)))

    (define-record-type <op>
      (make-op name type)
      op?
      (name op-name)
      (type op-type))

    (define *unary-ops*
      `((+ . ,(make-op "+"
            (lambda (to)
              (values 'signed+unsigned+doublish 'double))))
        (- . ,(make-op "-"
            (lambda (to)
              (case to
                ((double doublish) (values 'doublish 'double))
                (else (values 'int 'intish))))))
        (~ . ,(make-op "~"
            (lambda (to)
              (values 'intish 'signed))))
        (not . ,(make-op "!"
            (lambda (to)
              (values 'int 'int))))))
 
    (emit
      (compile-module
        `(module "RapidScheme"
          (signed a)
          (unsigned c)
          (double b 0.0)
          (math imul "imul")
          (stdlib NaN "NaN")
          (heap i8 "Int8Array")
          (heap f32 "Float32Array")
          (foreign int heap-size "heapSize")
          (function signed alloc ((int i))
            (double j 1.0)
            (label l1
              (begin
                (break l1)))
                
            (switch i
              (case 0
                (continue))
              (default
                (break)))
            
            (ref f32 8)
            (ref f32 a)
            (ref i8 a)
            
            (+ 1.2)
            
            (return 42))
       
          (return a))))
      
      
      
      ))
