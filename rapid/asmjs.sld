(define-library (rapid asmjs)
  (export
    boolean true false
    global-reg
    environment-ptr
    local-location
    local-value
    heap-location
    heap-value)
  (import (scheme base))
  (begin
     
    (define (boolean true?)
      (if true? "0x10001" "0x1"))
      
    (define (true)
      (boolean #t))
      
    (define (false)
      (boolean #f))
      
    (define (global-reg index)
      (string-append "g" (number->string index)))
     
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
      (location->value (heap-location pointer)))))

