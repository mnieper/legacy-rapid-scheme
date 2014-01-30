(define-library (rapid load)
  (export load load-file)
  (import (scheme base) (scheme read) (scheme file) (rapid library)) 
  (begin

    ; TODO Find better names for the procedure in this library.
    ;
    ; TODO Possibly use streams instead of loading the whole file into one huge
    ; list.
    ;
    ; FIXME Catch possible file errors.
    ;
    ; TODO: Refactor the load procedure in several smaller procedures

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
            `((define-library ()
                ,@(begin
                  (unless (import-declaration? datum)
                    (error "program does not begin with import declaration" datum))
                  (let loop ((datum datum))
                    (if (eof-object? datum)
                      `((begin))
                      (if (import-declaration? datum)
                        (cons datum (loop (read)))
                        `((begin ,@(cons datum (read-file))))))))))))))
              
    (define (read-file)
      (let ((datum (read)))
        (if (eof-object? datum)
          '()
          `(,datum . ,(read-file)))))
    
     
    (define (load-file filename)
      (with-input-from-file filename
        (read-file)))))
