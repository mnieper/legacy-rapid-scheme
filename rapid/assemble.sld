(define-library (rapid assemble)
  (export
    assemble import
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

    (define (assemble global-count imports)
      (parameterize ((current-output-port (open-output-string)))
        (emit (rapid-module global-count imports))
        (get-output-string (current-output-port))))

    (define (rapid-module global-count imports)
      (module "RapidModule"
        `(,@(globals global-count) ,@(import-declarations imports))
        `(,(run))
        '()
        (return (variable "run"))))
        
    (define (globals global-count)
      (let loop ((i 0))
        (if (= i global-count)
          '()
          `(,(variable-declaration (cadr (global-reg i)) (literal 0)) . ,(loop (+ i 1))))))

    (define (import type identifier)
      (vector type identifier))
      
    (define (import-type import)
      (vector-ref import 0))

    (define (import-identifier import)
      (vector-ref import 1))

    (define (import-declarations imports)
      (let loop ((i 0) (imports imports))
        (if (null? imports)
          '()
          (cons
            (let* ((import (car imports)) (identifier (import-identifier import)))
              (case (import-type import)
                ((math) (import-math (extern i) identifier))
                ((stdlib) (import-math (extern i) identifier))
                ((function) (foreign-function (extern i) identifier))
                ((int) (foreign-int (extern i) identifier))
                ((double) (foreign-double (extern i) identifier))))
            (loop (+ i 1) (cdr imports))))))

    (define (extern i)
      (variable (string-append "x" (number->string i))))

    (define (run)
      (function "run" '()
        (while (literal 1)
          (switch (signed (code-reg))))))

    (define (ff-imul) "imul")
    (define (ff-exit) "exit")
    (define (ff-write-string) "writeString")
    (define (ff-memory-error) "memoryError")
    (define (ff-call-error) "callError")
    (define (ff-application-error) "applicationError")

    (define (global-reg index)
      (variable (string-append "g" (number->string index))))

    (define (code-reg)
      (variable "i"))
     
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
      (number->string expression)) ; FIXME
    
    (define (boolean true?)
      (if true? "0x10001" "0x1"))
    
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

