(define-library (rapid standard-libraries)
  (export expand)
  (import
    (scheme base)
    (scheme case-lambda)
    (rapid error)
    (rapid sequence)
    (rapid library))
  (begin

    (define (expand gensym)
      (let-values (((code export-set) (expand-scheme-base gensym)))
        (values code (list export-set))))

    (define (expand-scheme-base gensym)
      (let* ((library (make-library '(scheme base) gensym))
             (body
              (list
                (expand-operator-definition library '+ 0 #t)
                (expand-operator-definition library 'apply 1 #t))))
        (export! library '+)
        (export! library 'apply)
        (values (make-sequence body) (export-set library))))

    (define (expand-operator-definition library operator len rest?)
      (define symbol (gensym library))
      (define params
        (do ((params '() (cons (gensym library) params))
             (i 0 (+ i 1)))
          ((= i len) params)))
      (define! library
        (make-binding operator
          (case-lambda
            ((library) symbol)
            ((library args)
              (with-context ("in application of operator" operator)
                (let ((arg-count (length args)))
                  (if rest?
                    (when (< arg-count len)
                      (raise-compile-error "not enough arguments" arg-count))
                    (when (not (= arg-count len))
                      (raise-compile-error "wrong number of arguments" arg-count))))
                `(,operator
                  ,@(map (lambda (arg) (expand-command-or-definition arg library)) args)))))))
      `(define ,symbol
        ,(if rest?
          (let ((rest (gensym library)))
            (if (null? params)          
              `(lambda ,rest (apply ,operator ,rest))
              `(lambda (,@params . ,rest) (apply ,operator ,@params ,rest))))
          `(lambda (,@params) (,operator ,@params)))))


    ))
