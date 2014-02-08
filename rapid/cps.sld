(define-library (rapid cps)
  (export cps)
  (import
    (scheme base)
    (scheme write)
    (scheme cxr)
    (rapid base)
    (rapid program))
  (begin

    (define (atomic? expr)
      (or (atom? expr) (case-lambda? expr)))
      
    (define (cps program expression)
      
      (define gensym (program-gensym program))

      (define (cps expr k)
        (cond
          ((atom? expr) (cps-atom expr k))
          ((case-lambda? expr) (cps-case-lambda (cdr expr) k))
          ((set!? expr) (cps-op 'set! (cdr expr) k))
          ((if? expr) (cps-if (cadr expr) (caddr expr) (cadddr expr) k))
          ((op expr) (cps-op (car expr) (cdr expr) k))
          ((pair? expr) (cps-application expr k))
          (else (error "cps: unknown expression type" expr))))

      (define (cps-atom expr k)
        (if (not (procedure? k))
          (cps-atom expr (lambda (a) `(,k ,a)))
          (k
            (if (eq? expr 'call/cc)
              (let ((cc (gensym)) (proc (gensym))
                    (cc1 (gensym))
                    (value (gensym)))
                `(case-lambda ((,cc ,proc) (,proc ,cc (case-lambda ((,cc1 ,value) (,cc ,value)))))))
              expr))))

      (define (cps-case-lambda clauses k)
        (if (not (procedure? k))
          (cps-case-lambda clauses (lambda (a) `(,k ,a)))
          (k `(case-lambda
              ,@(let loop ((clauses clauses)) ; TODO Rewrite to for-each
                (if (null? clauses)
                  '()
                  (let ((clause (car clauses)))
                    (let ((formals (car clause)) (body (cdr clause)) (c (gensym)))
                      (let-values ((a* (cps-body body c)))
                        `(((,c . ,formals) . ,a*) . ,(loop (cdr clauses))))))))))))
          
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
              `((case-lambda ((,c) ,(cps-if pred con alt c))) ,k)))))

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
            `(case-lambda ((,d) . ,body)))))
      
      (cps expression
        (lambda (a)
          (let ((v (gensym)))
            `(,a (case-lambda ((,v) (exit ,v))))))))))

