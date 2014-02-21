(define-library (rapid module)
  (export
    module at parent-frame-ptr arg local global
    number boolean true false box-i32 unbox-i32 string-const
    conditional)
  (import (scheme base))
  (begin

    (define (at ptr) `(ref i32 ,ptr))
    
    (define (parent-frame-ptr fp) (at `(+ ,fp 4)))
    
    (define (arg fp displacement) (at `(+ ,fp ,(+ 8 (* 4 displacement)))))
    
    (define (local depth displacement)
      (let loop ((fp 'env-ptr) (depth depth))
        (if (= depth 0)
          (arg fp displacement)
          (loop (parent-frame-ptr fp) (- depth 1)))))   

    (define (global i)
      (string->symbol (string-append "global-" (number->string i))))

    (define (number expr)
      expr) ; FIXME Allow larger numbers than 2^31.
      
    (define (boolean true?)
      (if true? #x00010001 #x00000001))
      
    (define true (boolean #t))
    
    (define false (boolean #f))

    (define (box-i32 i32)
      `(<< ,i32 1))
      
    (define (unbox-i32 i32) 
      `(>> ,i32 1))
      
    (define (string-const expr)
      (let* ((expr (string->vector expr 0)) (len (vector-length expr)))
        `(begin
          (set! data-ptr (alloc ,(+ 8 (* 4 len))))
          (set! ,(at 'data-ptr) ,*string-tag*)
          (set! ,(at '(+ data-ptr 4)) ,len) .
          ,(let loop ((i 0))
            (if (= i len)
              `(data-ptr)
              `((set! ,(at `(+ data-ptr ,(+ 8 (* 4 i))))
                  ,(char->integer (vector-ref expr i))) .
                ,(loop (+ i 1))))))))

    (define (conditional test consequent alternate)
      `(if (not= ,test ,false) ,consequent ,alternate))

    (define (module global-count block*)
      (let ((globals (globals global-count)) (blocks (blocks block*)))
        (let-syntax ((module (syntax-rules ()
                ((module . rest) `(module . rest)))))
          (include "rapid/module.scm"))))

    (define *string-tag* 0)
    
    (define *proc-tag* #x3)
    
    (define (globals count)
      (let loop ((i 0))
        (if (= i count)
          '()
          `((signed ,(global i)) . ,(loop (+ i 1))))))

    (define (blocks block*)
      (let loop ((i 0) (block* block*))
        (if (null? block*)
          '()
          `((case ,i) . ,(car block*))
          (loop (+ i 1) (cdr block*)))))))
