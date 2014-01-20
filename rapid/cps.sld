(define-library (rapid cps)
  (export cps)
  (import (scheme base) (scheme write) (scheme cxr) (rapid base))
  (begin

    (define (cps expr k)
      (cond
        ((atom? expr) (cps-atom expr k))
        ((lambda? expr) (cps-lambda (cadr expr) (cddr expr) k))
        ((set!? expr) (cps-op 'set! (cdr expr) k))
        ((if? expr) (cps-if (cadr expr) (caddr expr) (cadddr expr) k))
        ((op expr) (cps-op (car expr) (cdr expr) k))
        ((pair? expr) (cps-application expr k))
        (else (error "cps: unknown expression type" expr))))

    (define (atomic? expr)
      (or (atom? expr) (lambda? expr)))
  
    (define (cps-atom expr k)
      (if (not (procedure? k))
        (cps-atom expr (lambda (a) `(,k ,a)))
        (k
          (if (eq? expr 'call/cc)
            (let ((cc (gensym "call/cc-cc-")) (proc (gensym "call/cc-proc-"))
                  (cc1 (gensym "call/cc-cc1-"))
                  (value (gensym "call/cc-value-")))
              `(lambda (,cc ,proc) (,proc ,cc (lambda (,cc1 ,value) (,cc ,value)))))
            expr))))

    (define (cps-lambda vars body k)
      (if (not (procedure? k))
        (cps-lambda vars body (lambda (a) `(,k ,a)))
        (let ((c (gensym "c")))
          (let-values ((a* (cps-body body c)))
            (k `(lambda (,c . ,vars) . ,a*))))))
          
    (define (cps-body body k)
      (let ((expr (car body)) (tail (cdr body)))  
        (cps expr
          (if (null? tail)
            k
            (lambda (a)
              (let-values ((a* (cps-body tail k)))
                (if (atomic? a)
                  (apply values a*)
                  (apply values a a*))))))))

    (define (cps-if pred con alt k)
      (if (procedure? k)
        (cps-if pred con alt (continuation k))    
        (if (atom? k)
          (cps pred
            (lambda (a)
              `(if ,a ,(cps con k) ,(cps alt k))))
          (let ((c (gensym)))
            `((lambda (,c) ,(cps-if pred con alt c)) ,k)))))

    (define (cps-op op args k)
      (if (not (procedure? k))
        (cps-op op args (lambda (a) `(,k ,a)))
        (cps-terms args
          (lambda (t*)
            (k `(,op ,@t*))))))

    (define (cps-application expr k)
      (if (procedure? k)
        (cps-application expr (continuation k))
        (cps-terms expr
          (lambda (t*)
            `(,(car t*) ,k . ,(cdr t*))))))
    
    (define (cps-terms e* k)
      (if (null? e*)
        (k '())
        (cps (car e*)
          (lambda (a)
            (cps-terms (cdr e*)
              (lambda (a*)
                (k (cons a a*))))))))

    (define (continuation k)
      (let ((d (gensym)))
        (let-values ((body (k d)))
          `(lambda (,d) . ,body))))))

