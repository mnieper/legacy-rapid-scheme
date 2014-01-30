(define-library (rapid sequence)
  (export make-sequence empty-sequence sequence->body sequence?)
  (import (scheme base) (rapid base))
  (begin
  
    (define (make-sequence body)
      (cons 'begin body))
  
    (define (sequence? sequence)
      (tagged-pair? sequence 'begin))
      
    (define (sequence-body sequence)
      (cdr sequence))
    
    (define (empty-sequence)
      (make-sequence '()))
    
    (define (sequence->body sequence)
      (let loop ((body (sequence-body sequence)) (tail (list)))
        (if (null? body)
          tail
          (let ((head (car body)) (tail (loop (cdr body) tail)))
            (if (sequence? head)
              (loop (sequence-body head) tail)
              (cons head tail))))))))
