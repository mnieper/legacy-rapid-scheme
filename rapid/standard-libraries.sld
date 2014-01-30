(define-library (rapid standard-libraries)
  (export expand)
  (import
    (scheme base)
    (scheme case-lambda)
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

    (define (expand-operator-definition library operator length rest?)
      (define symbol (gensym library))
      (define params
        (do ((params '() (cons (gensym library) params))
             (i 0 (+ i 1)))
          ((= i length) params)))
      (define! library
        (make-binding operator
          (case-lambda
            ((library) symbol)
            ((library args)
              `(,operator
                ; TODO Signal an error if the number of arguments in not correct
                ; with respect to length and rest?
                ,@(map (lambda (arg) (expand-command-or-definition arg library)) args))))))
      `(define ,symbol
        ,(if rest?
          (let ((rest (gensym library)))
            (if (null? params)          
              `(lambda ,rest (apply ,operator ,rest))
              `(lambda (,@params . ,rest) (apply ,operator ,@params ,rest))))
          `(lambda (,@params) (,operator ,@params)))))


    ))
