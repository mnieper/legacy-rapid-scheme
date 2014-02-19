(define-library (rapid assemble)
  (export
    assemble
    boolean true false number string-const conditional
    global-reg
    local-location
    local-value
    heap-location
    heap-value)
  (import
    (scheme base)
    (rapid asmjs))
  (begin

    (define (assemble global-count blocks)
      (parameterize ((current-output-port (open-output-string)))
        (emit (link-module (rapid-module global-count blocks)))
        (get-output-string (current-output-port))))

    (define (rapid-module global-count blocks)
      (js-module (js-id "RapidModule")
        (append (globals global-count) (variables) (imports) (ffi))
        (cons (main-function blocks) (functions))
        (list)
        (js-return 'main)))

    (define (globals global-count)
      (let loop ((i 0))
        (if (= i global-count)
          '()
          `(,(js-signed-var (global-reg i)) . ,(loop (+ i 1))))))

    (define (variables)
      (list
        (js-signed-var 'aux-reg)
        (js-signed-var 'code-reg)
        (js-signed-var 'env-ptr)
        (js-signed-var 'free-ptr)
        (js-signed-var 'data-ptr)
        (js-heap-view 'h32 "Uint32Array")
        (js-heap-view 'hf64 "Float64Array")))

    (define (imports)
      (list
        (js-math-import 'imul "imul")))

    (define (ffi)
      (list
        (js-foreign-signed 'heap-size "heapSize")
        (js-foreign-function 'application-error "applicationError")
        (js-foreign-function 'memory-error "memoryError")
        (js-foreign-function 'call-error "callError")))

    ; Alternatively: use a library and export functions
    (define (functions)
      (let
        ((set! js-assignment)
          (function js-function)
          (call js-call)
          (if js-if)
          (begin js-block)
          (+ js-+)
          (* js-*)
          (/ js-/)
          (truncate-remainder js-%)
          (or js-or)
          (& js-&)
          (^ js-^)
          (<< js-<<)
          (>> js->>)
          (>>> js->>>)          
          (< js-<)
          (<= js-<=)
          (>= js->=)
          (= js-==)
          (not= js-!=)
          (- js--))
        (letrec-syntax
          ((for
              (syntax-rules ()
                ((for init* test* update* . body) (js-for (list . init*) (list . test*) (list . update*) . body))))
            (var
              (syntax-rules ()
                ((var name 0) (js-signed-var 'name))
                ((var name 0.0) (js-double-var 'name))))
            (double
              (syntax-rules ()
                ((double name) (js-double-var 'double))))
            (return
              (syntax-rules (signed double)
                ((return signed val) (js-return (js-signed val)))
                ((return double val) (js-return (js-double val)))))
            (functions
              (syntax-rules ()
                ((functions . f*) (list . f*)))))
          (include "rapid/functions.scm"))))

    (define (main-function blocks)
      (js-function 'main '()
        (js-while (number 1)
          (apply js-switch (js-signed 'code-reg)
            (append
              (let loop ((i 0) (blocks blocks))
                (if (null? blocks)
                  '()
                  (cons 
                    (js-case (number i) (car blocks))
                    (loop (+ i 1) (cdr blocks))))))))))

    (define (global-reg index)
      (string->symbol (string-append "global-" (number->string index))))
    
    (define (heap-location ptr)
      (js-member 'h32 (js->> ptr (number 2))))
      
    (define (heap-value ptr)
      (js-signed (heap-location ptr)))

    (define (parent-frame-ptr fp)
      (heap-value (js-+ fp (number 4))))
      
    (define (arg-location fp displacement)
      (heap-location (js-+ fp (+ 8 (* 4 displacement)))))
      
    (define (arg-value fp displacement)
      (js-signed (arg-location fp displacement)))
      
    (define (local-location depth displacement)
      (let loop ((fp 'env-ptr) (depth depth))
        (if (= depth 0)
          (arg-location fp displacement)
          (loop (parent-frame-ptr fp) (- depth 1)))))
          
    (define (local-value depth displacement)
      (js-signed (local-location depth displacement)))
      
    (define (number expression)
      (js-number expression)) ; A Scheme number is generally too large for a JS number.
    
    (define (boolean true?)
      (if true? (number #x00010001) (number #x0000001)))

    (define true (boolean #t))
      
    (define false (boolean #f))

    (define (js-integer->value int)
      (js-<< int 1))
      
    (define (js-value->integer val)
      (js->> val 1))

    (define (box-integer int)
      (js-<< int 1))
      
    (define (unbox-integer val)
      (js->> val 1)) 

    (define string-tag 0)

    (define (string-const expr)
      (let* ((expr (string->vector 0)) (len (vector-length expr)))
        (js-signed
          (apply js-expression
            (js-assignment 'data-ptr
              (js-signed (js-call 'alloc (+ 8 (* 4 len)))))
            (js-assignment (heap-location 'data-ptr) string-tag)
            (js-assignment (heap-location (js-+ 'data-ptr 4)) (number len))
            (let loop ((i 0))
              (if (= i len)
                '(data-reg)
                (cons (js-assignment (heap-location (js-+ 'data-ptr (+ 8 (* 4 i)))) (char->integer (vector-ref expr i)))
                  (loop (+ i 1)))))))))

    (define (conditional test consequent alternate)
      (js-if (js-!= (js-signed test) false) consequent alternate))))
