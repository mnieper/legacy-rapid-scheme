(define-library (rapid assemble)
  ;
  ; TODO Refactor procedures in smaller pieces.
  ;
  ; TODO Rename compiles into assembles.
  ;
  (export assemble-module make-global-var make-local-var make-label)
  (import (scheme base) (scheme case-lambda) (scheme inexact) (rapid asmjs) (scheme write))
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

    (define (assemble-module mod)
      (new-environment
        (for-each globals-update! (list-tail mod 2))
        `(var (id ,(list-ref mod 1))
          (function (id "") ((id "s") (id "f") (id "h"))
            (statement (string "use asm")) .
            ,(map compile-decl (list-tail mod 2))))))

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
          (case (list-ref decl 1)
            ((signed unsigned double) (global-set! (list-ref decl 2) (list-ref decl 1))) ; PROBLEM : Is int, but has to be coerced; like parameter???
            ((function)
              (global-set! (list-ref decl 3) (list (list-ref decl 2) (list 'extern))))
            (else (error "unknown foreign type" (list-ref decl 1)))))
        ((function)
          (global-set! (list-ref decl 2)
            (list (list-ref decl 1)
              (map (lambda (param) (case (list-ref param 0)
                    ((signed unsigned) 'int)
                    ((double) 'double)
                    (else (error globals-update! "invalid parameter type" (list-ref param 0)))))
                (list-ref decl 3)))))
        ((table)
          (global-set! (list-ref decl 1) (table-type (length (list-ref decl 2)) (type (car (list-ref decl 2))))))))
    
    (define (compile-decl decl)
      (case (car decl)
        ((signed unsigned double)
          `(var ,(id (list-ref decl 1)) ,(apply initializer (list-ref decl 0)
              (list-tail decl 2))))
        ((stdlib)
          `(var ,(id (list-ref decl 1))
            (member (id "s") ,(list-ref decl 2))))
        ((math)
          `(var ,(id (list-ref decl 1))
            (member (member (id "s") "Math") ,(list-ref decl 2))))
        ((heap)
          `(var ,(id (list-ref decl 1))
            (new (member (id "s") ,(list-ref decl 2)) (id "h"))))
        ((foreign) 
          (case (list-ref decl 1)
            ((signed unsigned double)
              `(var ,(id (list-ref decl 2))
                ,(coerce 'void (list-ref decl 1)
                  `(member (id "f") ,(list-ref decl 3)))))
            ((function)
              `(var ,(id (list-ref decl 3))
                (member (id "f") ,(list-ref decl 4)))))) 
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
                      ,(coerce 'void (case (list-ref param 0)
                          ((signed unsigned) 'signed) ((double) 'double))
                        (value-id val)))))
                (list-ref decl 3))
              ,@(compile-body (list-ref decl 1) (list-tail decl 4))))) 
        ((table)
          `(var ,(id (list-ref decl 1))
            (array ,@(map (lambda (f) (id f)) (list-ref decl 2)) .
              ,(make-list (- (table-size (type (list-ref decl 1))) (length (list-ref decl 2)))
                (id (car (list-ref decl 2)))))))
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
                        ,(apply initializer (list-ref stmt 0) (list-tail stmt 2))))
                    ((return)
                      (if (and (null? (cdr stmt*)) (> (length stmt) 1))
                        (let ((arg (coerce-expression 'void (list-ref stmt 1))))
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
                `(,(coerce-expression return-type (list-ref stmt 1)))
                '())))
          ((begin)
            `(block . ,(compile-statement* return-type (cdr stmt))))
          ((if)
            `(if ,(coerce-expression 'int (list-ref stmt 1))
              ,(compile-statement return-type (list-ref stmt 2)) .
              ,(compile-statement* return-type (list-tail stmt 3))))
          ((while)
            `(while ,(coerce-expression 'int (list-ref stmt 1))
              ,(compile-block return-type (list-tail stmt 2))))
          ((do)
            `(do ,(coerce-expression 'int (list-ref stmt 1))
              ,(compile-block return-type (list-tail stmt 2))))
          ((for)
            `(for ,(coerce-expression* 'void (list-ref stmt 1))
              ,(coerce-expression* 'int (list-ref stmt 2))
              ,(coerce-expression* 'void (list-ref stmt 3))
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
            `(switch ,(coerce-expression 'signed (list-ref stmt 1)) .
              ,(compile-statement* return-type (list-tail stmt 2))))
          ((case)
            `(case ,(coerce-expression 'signed (list-ref stmt 1)) .
              ,(compile-statement* return-type (list-tail stmt 2))))
          ((default)
            `(default . ,(compile-statement* return-type (list-tail stmt 1))))          
          (else `(statement ,(coerce-expression 'void stmt))))
        `(statement ,(coerce-expression 'void stmt))))
    
    (define (compile-block return-type stmt*)
      (if (null? (cdr stmt*))
        (compile-statement return-type (car stmt*))
        `(block ,@(compile-statement* return-type stmt*)))) 
    
    (define (compile-statement* return-type stmt*)
      (map (lambda (stmt) (compile-statement return-type stmt)) stmt*))

    (define (compile-expression expr)
      (cond
        ((number? expr)
          (let ((js `(number ,expr)))
            (cond
              ((inexact? expr) (values js 'double 'double)) 
              ((negative? expr) (values js 'signed 'signed))
              ((< expr #x80000000) (values js 'fixnum 'signed))
              (else (values js 'unsigned 'unsigned)))))
        ((symbol? expr) 
          (let ((val (value expr)))
            (case (value-type val)
              ((signed) (values (value-id val) 'int 'signed))
              ((unsigned) (values (value-id val) 'int 'unsigned))
              ((double) (values (value-id val) 'double 'double))
              (else (error "unknown type" expr (value-type val))))))
        (else
          (case (car expr)
            ((begin)
              (let ((expr* (reverse (cdr expr))))
                (let-values (((js actual inferred) (compile-expression (car expr*))))
                  (values
                    `(expression .
                      ,(let loop ((js* (list js)) (expr* (cdr expr*)))
                        (if (null? expr*)
                          js*
                          (let-values (((js actual inferred) (compile-expression (car expr*))))
                            (loop (cons js js*) (cdr expr*))))))
                    actual inferred))))
            ((ref)            
              (let ((val (value (list-ref expr 1))))
                (let ((type (value-type val)) (id (value-id val)))
                  (values            
                    (cond
                      ((number? (list-ref expr 2))    
                        `(member ,id
                            (number ,(/ (list-ref expr 2) (view-size type)))))
                      ((= (view-size type) 1)
                        `(member ,id
                          ,(coerce-expression 'int (list-ref expr 2))))
                      (else
                        `(member ,id
                          (binary ">>"
                            ,(coerce-expression 'intish (list-ref expr 2))
                            (number ,(view-shift type))))))
                    (view-actual-type type) (view-inferred-type type)))))
            ((set!)
              (let ((lhs (list-ref expr 1)) (rhs (list-ref expr 2)))
                (if (and (pair? lhs) (eq? (car lhs) 'ref))
                  (let-values (((js actual inferred) (compile-expression lhs)))
                    (values `(assignment ,js ,(coerce-expression actual rhs))
                      actual inferred))
                  (let ((val (value lhs)))                      
                    (values `(assignment ,(value-id val)
                        ,(coerce-expression (value-type val) rhs))
                      (value-type val) (value-type val))))))
            ((signed unsigned double)    
              (let-values (((js actual inferred)
                    (compile-expression (list-ref expr 1))))
                (values (coerce actual inferred js) inferred (list-ref expr 0))))                  
            ((truncate)
              (values
                `(unary "~"
                  (unary "~" ,(coerce-expression 'double (list-ref expr 1))))
                'signed 'signed))
            ((-) 
              (if (> (length expr) 2)
                (compile-binary expr)
                (compile-unary expr)))
            ((~ not)
              (compile-unary expr))            
            ((+) (compile-sum (cdr expr)))
            ((* / remainder << >> >>> < > <= >= = not= and ^ or)
              (if (> (length expr) 3)
                (let ((op (list-ref expr 0)))
                  (compile-expression
                    `(,op (,op ,(list-ref expr 1) ,(list-ref expr 2)) .
                      ,(list-tail expr 3))))
                (compile-binary expr)))
            ((if)
              (let-values (((js actual inferred)
                    (compile-expression (list-ref expr 2))))
                (case inferred
                  ((signed unsigned)
                    (values
                      `(conditional
                        ,(coerce-expression 'int (list-ref expr 1))
                        ,(coerce actual 'int js)
                        ,(coerce-expression 'int (list-ref expr 3)))
                      'int inferred))
                  ((double)
                    (values
                      `(conditional
                        ,(coerce-expression 'int (list-ref expr 1))
                        ,(coerce actual 'double js)
                        ,(coerce-expression 'double (list-ref expr 3)))
                      'double 'double))
                  (else (error compile-expression "unknown type" inferred)))))
            (else
              (compile-application (car expr) (cdr expr)))))))

    (define (compile-application proc arg*)
      (let* ((val (value proc)) (type (value-type val)))
        (cond
          ((table? type)
            (call (table-function-type type)
              `(member ,(value-id val)
                (binary "&" ,(coerce-expression 'int (car arg*))
                  (number ,(- (table-size type) 1))))
              (cdr arg*)))
          ((not (pair? type)) (error proc))  ; XXX Should report this error
          ((pair? (car type))
            (let-values (((js actual inferred) (compile-expression (car arg*))))
              (call (cdr (or (assq inferred type) (error compile-application "type mismatch" proc inferred)))
                (value-id val) arg*)))
          (else
            (call type (value-id val) arg*)))))

    (define (call type proc arg*)
      (let ((js
            `(call ,proc .
              ,(let loop ((arg* arg*) (type* (list-ref type 1)))
                (if (null? arg*)
                  (list)
                    (cons (coerce-expression (car type*) (car arg*))
                    (loop (cdr arg*)
                      (if (null? (cdr type*)) type* (cdr type*)))))))))
        (case (car type)
          ((signed)
            (values `(binary "|" ,js (number 0)) 'signed 'signed))
          ((double)
            (values `(unary "+" ,js) 'double 'double))
          (else
            (values js 'void 'void)))))

    (define (compile-sum arg*)
      (let-values (((js actual inferred) (compile-expression (car arg*))))
        (values
          (let loop ((js (coerce actual 'int js)) (arg* (cdr arg*)))
            (if (null? arg*)
              js
              (loop
                (case inferred
                  ((signed unsigned)
                    (if (and (pair? (car arg*)) (eq? (caar arg*) '-))
                      `(binary "-" ,js ,(coerce-expression 'int (cadr (car arg*))))
                      `(binary "+" ,js ,(coerce-expression 'int (car arg*)))))
                  (else
                    `(binary "+" ,js ,(coerce-expression 'double (car arg*)))))
                (cdr arg*))))
          (case inferred ((signed unsigned) 'intish) (else 'double))        
          inferred)))

    (define (compile-binary expr)
      (unless (= 3 (length expr))
        (error compile-binary "malformed binary expression" expr))
      (let ((op (list-ref expr 0)) (left (list-ref expr 1)) (right (list-ref expr 2)))
        (case op
          ((*) 
            (if (exact? left)
              (let-values (((js actual inferred) (compile-expression right)))
                (values
                  `(binary "*" (number ,(list-ref expr 1)) ,(coerce actual 'int js))
                  'intish inferred))
              (compile-generic-binary op left right)))
          (else (compile-generic-binary op left right)))))

    (define (compile-generic-binary binary left right)
      (let ((op (binary-op binary)))
        (let-values (((js actual inferred)
              (compile-expression left)))
          (let ((type (cdr
                (or (assq inferred (op-types op))
                  (error compile-binary "type mismatch" binary left right)))))
            (values
              `(binary ,(op-name op) ,(coerce actual (list-ref type 0) js)
                ,(coerce-expression (list-ref type 0) right))
              (list-ref type 1) (list-ref type 2))))))
  
    (define (compile-unary expr)
      (let ((op (unary-op (list-ref expr 0))))
        (let-values (((js actual inferred)
              (compile-expression (list-ref expr 1))))
          (let ((type (cdr
                  (or (assq inferred (op-types op))
                    (error compile-unary "type mismatch" expr)))))
            (values
              `(unary ,(op-name op) ,(coerce actual (list-ref type 0) js))
              (list-ref type 1) (list-ref type 2))))))

    (define (coerce-expression* expected-type expr*)
      (map (lambda (expr) (coerce-expression expected-type expr)) expr*))

    (define (coerce-expression expected-type expr)
      (let-values (((js actual inferred) (compile-expression expr)))
        (coerce actual expected-type js)))

    (define (coerce from to js)
      (case to
        ((extern)
          (case from
            ((unsigned int intish) `(binary "|" ,js (number 0)))
            ((doublish) `(unary "+" ,js))
            ((fixnum signed double) js)
            (else (error coerce "cannot be coerced to extern" from))))
        ((unsigned)
          (case from
            ((double doublish intish) `(binary ">>>" ,js (number 0)))
            ((unsigned signed fixnum int) js)
            ((void `(binary "|" ,js 0)))
            (else (error coerce "cannot be coerced to unsigned" from))))
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
        ((doublish)
          (case from
            ((doublish) js)
            (else (coerce from 'double js))))
        ((double)
          (case from
            ((double) js)
            ((unsigned signed) `(unary "+" ,js))
            (else (error coerce "cannot be coerced to double" from))))
        ((void) js)
        (else (error coerce "illegal type to coerce to" to))))
    
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
        (string-append "x" (number->string i)))))

    (define make-local-var
      (make-parameter (lambda (i)
        (string-append "y" (number->string i)))))
    
    (define make-label
      (make-parameter (lambda (i)
        (string-append "a" (number->string i)))))
    
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

    (define-record-type <table>
      (make-table size function-type)
      table?
      (size table-size)
      (function-type table-function-type))

    (define (table-type size function-type)
      (make-table  (exact (expt 2 (ceiling (log size 2))))
        function-type))

    (define-record-type <view>
      (make-view size actual inferred shift)
      view?
      (size view-size)
      (actual view-actual-type)
      (inferred view-inferred-type)
      (shift view-shift))

    (define *heap-types*
      `(("Uint8Array" . ,(make-view 1 'intish 'unsigned 0))
        ("Int8Array" . ,(make-view 1 'intish 'signed 0))
        ("Uint16Array" . ,(make-view 2 'intish 'unsigned 1))
        ("Int16Array" . ,(make-view 2 'intish 'signed 1))
        ("Uint32Array" . ,(make-view 4 'intish 'unsigned 2))
        ("Int32Array" . ,(make-view 4 'intish 'signed 2))
        ("Float32Array" . ,(make-view 4 'doublish 'double 2))
        ("Float64Array" . ,(make-view 8 'doublish 'double 3))))

    (define (math-type s)
      (cdr (assoc s *math-types*)))
      
    (define *math-types*
      `(("acos" . (double (doublish)))
        ("asin" . (double (doublish)))
        ("atab" . (double (doublish)))
        ("cos" . (double (doublish)))
        ("sin" . (double (doublish)))
        ("tan" . (double (doublish)))
        ("ceil" . (double (doublish)))
        ("floor" . (double (doublish)))
        ("exp" . (double (doublish)))
        ("log" . (double (doublish)))
        ("sqrt" . (double (doublish)))
        ("abs" . ((signed signed (signed)) (double double (doublish))))
        ("atan2" . (double (doublish doublish)))
        ("pow" . (double (doublish doublish)))
        ("imul" . (signed (int int)))
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

    (define (binary-op s)
      (cdr (or (assq s *binary-ops*))))

    (define-record-type <op>
      (make-op name types)
      op?
      (name op-name)
      (types op-types))

    (define *unary-ops*
      `((- . ,(make-op "-" '((signed int intish signed) (double doublish double double))))   ; input from actual inferred 
        (~ . ,(make-op "~" '((signed intish signed signed))))
        (not . ,(make-op "!" '((signed int int signed) (unsigned int int unsigned))))))
 
    (define *binary-ops*
      `((- . ,(make-op "-" '((double doublish double double))))
        (* . ,(make-op "*" '((double doublish double double))))
        (/ . ,(make-op "/" '((signed signed intish signed) (unsigned unsigned intish unsigned) (double doublish double double))))
        (remainder . ,(make-op "%" '((signed signed intish signed) (unsigned unsigned intish unsigneed) (double doublish double double))))
        (or . ,(make-op "|" '((signed intish signed signed) (unsigned intish signed signed))))
        (and . ,(make-op "&" '((signed intish signed signed) (unsigned intish signed signed))))
        (^ . ,(make-op "^" '((signed intish signed signed) (unsigned intish signed signed))))
        (<< . ,(make-op "<<" '((signed intish signed signed) (unsigned intish signed signed))))
        (>> . ,(make-op ">>" '((signed intish signed signed) (unsigned intish signed signed))))
        (>>> . ,(make-op ">>>" '((unsigned intish unsigned unsigned) (signed intish unsigned unsigned))))
        (< . ,(make-op "<" '((signed signed int unsigned))))  ; FIXME Add unsigned and double versions.
        (<= . ,(make-op "<=" '((signed signed int unsigned))))
        (> . ,(make-op ">" '((signed signed int unsigned))))
        (>= . ,(make-op ">=" '((signed signed int unsigned))))
        (= . ,(make-op "==" '((signed signed int unsigned))))
        (not= . ,(make-op "!=" '((signed signed int unsigned))))))))
