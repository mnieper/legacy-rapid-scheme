(define-library (rapid assemble)
  (export
    assemble 
    boolean true false number string
    global-reg
    environment-ptr
    local-location
    local-value
    heap-location
    heap-value)
  (import
    (scheme base)
    (only (rapid base) output-from)
    (rapid asmjs))
  (begin

    (define (assemble global-count)
      (parameterize ((current-output-port (open-output-string)))
        (emit (rapid-module global-count))
        (get-output-string (current-output-port))))

    (define variables '())

    (letrec-syntax ((define-global-vars
          (syntax-rules ()
            ((define-vars (type id name) global ...)
              (define id (js-id name))
              (set! globals `(,@globals ,(type id))))))
        (signed
          (syntax-rules ()
            ((signed id) (js-signed-var id))))
        (double
          (syntax-rules ()
            ((double id) (js-double-var id)))))
      (import "assemble/global-vars.scm"))

    (let

              
    
    (define imul (js-id "imul"))
    (define code-reg (js-id "i"))
    
    (define run (js-id "c"))
    (define free-ptr (js-id "f"))
    (define heap-size (js-id "z"))

    (define (variables) (list
        (js-signed-var code-reg)
        (js-signed-var free-ptr)
        ))

    (define (imports)
      (list
        (js-math-import imul "imul")
        (js-foreign-int heap-size "heapSize")))



    (letrec-syntax ((define-ffi
          (syntax-rules ()
            ((define-ffi ffi function ...)
              (define-ffi-aux ffi 0 '() function ...))))
        (define-ffi-aux
          (syntax-rules ()
            ((define-ffi-aux ffi count js*)
              (define (ffi) js*))
            ((define-ffi-aux ffi count js* (identifier foreign) function ...)
              (begin
                (define identifier (js-id (string-append "x" (number->string count))))
                (define-ffi-aux ffi (+ count 1) (cons (js-foreign-function identifier foreign) js*) function ...))))))
      (include "rapid/ffi.scm"))
 
    ; TODO: Move this into a library "define-functions"; let functions import this library and export everything else
    ; globals should also be defined in define-functions...
    ; TODO: All the lists and conses and macros below should be made by macros 
    
    (letrec-syntax ((define-functions
          (syntax-rules ()
            ((define-functions functions declaration ...)
              (define-functions-aux functions 0 '() declaration ...))))
        (define-functions-aux
          (syntax-rules ()
            ((define-functions-aux functions count js*)
              (define (functions) js*))
            ((define-functions-aux functions count js* (type identifier params . body) declaration ...)
              (begin
                (define identifier (js-id (string-append "q" (number->string count))))
                (define-functions-aux functions (+ count 1) (cons (function type identifier params . body) js*) declaration ...)))))
        (function
          (syntax-rules ()
            ((function type id params . body)
              (apply js-function id (function-aux 0 type params . body)))))
        (function-aux
          (syntax-rules (signed double)
            ((function-aux count type ((signed identifier) param ...) . body)
              (let ((identifier (js-id (string-append "l" (number->string count)))))
                (function-aux (+ count 1) type (param ... identifier) (js-signed-param identifier) . body)))
            ((function-aux count type ((double identifier) param ...) . body)
              (let ((identifier (js-id (string-append "l" (number->string count)))))
                (function-aux (+ count 1) type (param ... identifier) (js-double-param identifier) . body)))
            ((function-aux count type (identifier ...) . body)
              (cons (list identifier ...) (function-body count type . body)))))  ; splice list ... has to be done in function body
        (function-body
          (syntax-rules (signed double return)
            ((function-body count type (signed identifier) . body)
              (let ((identifier (js-id (string-append "l" (number->string count)))))
                (cons (js-signed-var identifier)
                  (function-body (+ count 1) type . body))))
            ((function-body count type (double identifier) . body)
              (let ((identifier (js-id (string-append "l" (number->string count)))))
                (cons (js-double-var identifier)
                  (function-body (+ count 1) type . body))))
            ((function-body count signed (return argument))
              (list (js-return (js-signed argument))))
            ((function-body count double (return argument))
              (list (js-return (js-double argument))))
            ((function-body count type statement . body)
              (cons statement (function-body count type . body)))
            ((function-body count type)
              (list)))))
      (include "rapid/functions.scm"))

    (define (rapid-module global-count)
      (js-module (js-id "RapidModule")
        `(,@(globals global-count) ,@(variables) ,@(imports) ,@(ffi))
        `(,@(functions) ,(run-function))
        '()
        (js-return run)))

    (define (globals global-count)
      (let loop ((i 0))
        (if (= i global-count)
          '()
          `(,(js-signed-var (global-reg i)) . ,(loop (+ i 1))))))

    (define (run-function)
      (js-function run '()
        (js-while (number 1)
          (js-switch (js-signed code-reg)))))

    (define (global-reg index)
      (js-id (string-append "g" (number->string index))))

    (define (environment-ptr)
      "e")
    
    (define (parent-frame-ptr frame-pointer)
      (heap-value (string-append "(" frame-pointer ")+4")))
      
    (define (arg-location frame-pointer displacement)
      (heap-location (string-append "(" frame-pointer ")+" (number->string (+ 8 (* displacement 4))))))

    (define (arg-value frame-pointer displacement)
      (location->value (arg-location frame-pointer displacement)))

    (define (local-location depth displacement)
      (let loop ((frame-pointer (environment-ptr)) (depth depth))
        (if (= depth 0)
          (arg-location frame-pointer displacement)
          (loop (parent-frame-ptr frame-pointer) (- depth 1)))))
          
    (define (local-value frame displacement)
      (location->value (local-location frame displacement)))
      
    (define (heap-location pointer)
      (string-append "h32[(" pointer ")>>2]"))
      
    (define (location->value location) 
      (string-append location "|0")) 
      
    (define (heap-value pointer)
      (location->value (heap-location pointer)))
      
    (define (empty-body) 
      "")
      
    (define (number expression)
      (js-number expression)) ; FIXME
    
    (define (boolean true?)
      (if true? (number #x00010001) (number #x0000001)))

    (define (true)
      (boolean #t))
      
    (define (false)
      (boolean #f))

    (define (asmjs-string expression)
      (output-from
        ;
        ; TODO: Simplify the following
        ; work with utf-32
        ;
        (define utf8 (string->utf8 expression))
        (write-string "(s=alloc(")
        (write-string (number->string (* 8 (quotient (+ 16 (bytevector-length utf8)) 8))))
        (write-string ")|0,h32[s>>2]=0x0,h32[s+4>>2]=")
        (write-string (number->string (bytevector-length utf8)))
        (do ((i 0 (+ i 1))) ((= i (bytevector-length utf8)))
          (write-string ",hu8[s+")
          (write-string (number->string (+ i 8)))
          (write-string "|0]=0x")
          (write-string (number->string (bytevector-u8-ref utf8 i) 16)))
        (write-string ",hu8[s+") (write-string (number->string (+ 8 (bytevector-length utf8)))) (write-string "|0]=0x0")
        (write-string ",s)|0")))

    (define (assignment variable expression)
      (string-append variable "=" expression))
      
    (define (call operation args)
      (string-append operation "("  ")|0"))  ; TODO

    (define (conditional test consequent alternate)
      (string-append "if((" test ")>>>0!=" (false) "){" consequent "}else{" alternate "}"))))

