(define-library (rapid module)
  (export
    module at arg local proc-label proc-frame frame-parent-frame proc-value? boolean-value
    frame-arg-count
    number boolean-value? true-value false-value small->value value->small string-const
    conditional *null-pointer*)
  (import (scheme base))
  (begin

    (define (at p) `(ref i32 ,p))

    (define *null-pointer* #x00000004)

    (define (small-value? val) `(= (and ,val #x00000001) #x00000000))
    (define (pointer-value? val) `(= (and ,val #x80000003) #x00000001))
    (define (boolean-value? val) `(= (and ,val #xfffffffb) #x00000002))
    (define (true-value? val) `(= (,val #x00000002)))
    (define (false-value? val) `(= (,val #x00000006)))
    (define (not-value? val) `(not= ,val #x00000006))
    
    (define (small->value i) `(<< ,i 1))
    (define (value->small val) `(>> ,val 1))
    (define (pointer->value p) `(or ,p #x00000001))
    (define (value->pointer p) `(and ,p #x7ffffffc))
    (define (boolean-value true?) (if true? #x00000002 #x00000006))
    (define true-value (boolean-value #t))
    (define false-value (boolean-value #f))    

    (define *tag-mask* #xe0000000)
    (define *size-mask* #x1fffffff)
    (define *frame-tag* #x00000000)
    (define *string-tag* #x40000000)
    (define *proc-tag* #x20000000)
    (define (frame-tag? tag) `(= (and ,tag ,*tag-mask*) ,*frame-tag*)) 
    (define (string-tag? tag) `(= (and ,tag ,*tag-mask*) ,*string-tag*)) 
    (define (frame-tag words) `(or ,words ,*frame-tag*))
    (define (string-tag words) `(or ,words ,*string-tag*))
    (define (tag ptr) (at ptr))

    (define proc-size 3)
    (define (proc-tag? tag) `(= (and ,tag ,*tag-mask*) ,*proc-tag*)) 
    (define (proc-value? val) `(and ,(pointer-value? val) ,(proc-tag? (tag val))))    
    (define (proc-label ptr) (at `(+ ,ptr 4)))
    (define (proc-frame ptr) (at `(+ ,ptr 8)))

    (define frame-header-size 2)
    (define (frame-arg-count frame) `(+ (and ,(tag frame) ,*size-mask*) ,(- frame-header-size))) 
    (define (frame-size arg-count) `(+ ,frame-header-size ,arg-count))
    (define (frame-parent-frame ptr) (at `(+ ,ptr 4)))
    (define (arg frame displacement)
      (at `(+ ,frame
          ,(if (number? displacement)
            (+ 8 (* 4 displacement))
            `(+ 8 (* 4 ,displacement))))))

    (define string-header-size 1)

    (define (local depth displacement)
      (let loop ((fp 'env-ptr) (depth depth))
        (if (= depth 0)
          (arg fp displacement)
          (loop (frame-parent-frame fp) (- depth 1)))))   

    ; Remove this.
    (define (global i)
      (string->symbol (string-append "global-" (number->string i))))

    (define (number expr)
      ; TODO: Check whether expr is a literal
      (small->value expr)) ; FIXME Allow larger numbers than 2^31.
      
    (define (string-const expr)
      (let* ((expr (string->vector expr 0)) (len (vector-length expr)))
        `(begin
          (set! data-ptr (new ,*string-tag* ,(+ len string-header-size))) .
          ,(let loop ((i 0))
            (if (= i len)
              `(data-ptr)
              `((set! ,(at `(+ data-ptr ,(+ 4 (* 4 i))))
                  ,(char->integer (vector-ref expr i))) .
                ,(loop (+ i 1))))))))

    (define (conditional test consequent . alternate*)
      `(if ,(not-value? test) ,consequent . ,alternate*))

    (define (module block*)
      (let ((blocks (blocks block*)))
        (let-syntax ((module (syntax-rules ()
                ((module . rest) `(module . rest)))))
          (include "rapid/module.scm"))))
    
    ; XXX Remove this.
    (define (globals count)
      (let loop ((i 0))
        (if (= i count)
          '()
          `((signed ,(global i)) . ,(loop (+ i 1))))))

    (define (blocks block*)
      (let loop ((block* block*))
        (if (null? block*)
          '()
          `((case ,(caar block*) . ,(cdar block*)) . ,
            (loop (cdr block*))))))))
