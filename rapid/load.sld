(define-library (rapid load)
  (export load)
  (import (scheme base) (scheme read) (scheme file) (rapid library)) 
  (begin

    (define (load)
      (let ((datum (read)))
        (when (eof-object? datum)
          (error "no program found"))
        (cond
          ((and (pair? datum) (eq? (car datum) 'define-library))
            (unless (and
                      (list? datum) (> (length datum) 1) (library-name? (cadr datum)))
              (error "invalid library definition" datum))
            (cons datum (load)))
          (else
            (list
              (cons datum (read-file)))))))
    
    (define (read-file)
      (let ((datum (read)))
        (if (eof-object? datum)
          '()
          `(,datum . ,(readfile)))))
    
     
    (define (load-file filename)
      (with-input-from file
        (read-file)))))
