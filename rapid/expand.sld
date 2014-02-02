(define-library (rapid expand)
  (export expand)
  (import (scheme base) (scheme case-lambda)
    (rapid base)
    (rapid sequence)
    (prefix (rapid standard-libraries) standard-libraries-)
    (rapid error)
    (rapid load)
    (rapid library)
    (rapid import-set))
  (begin

    (define (expand sources)
      (with-context ("while expanding")
        (define gensym (make-gensym))
        (sequence->body
          (let-values (((code export-sets)
                  (standard-libraries-expand gensym)))
            (make-sequence
              (cons code
                (let loop ((sources sources) (export-sets export-sets))
                  (if (null? sources)
                    '()
                    (let-values (((code export-set)
                          (expand-library gensym (car sources) export-sets)))
                      (cons code (loop (cdr sources) (cons export-set export-sets))))))))))))

    (define (expand-library gensym source export-sets)
      (let ((library-name (list-ref source 1)))
        (when (assoc library-name export-sets)
          (error "double defintion of library" library-name))
        (let* ((library (make-library library-name gensym))
               (code
                 (make-sequence
                   (map (lambda (declaration)
                       (expand-library-declaration declaration library export-sets))
                     (cddr source)))))
          (values code (export-set library)))))


    (define (expand-library-declaration declaration library export-sets)
      ; `declaration' is a library declaration as in 5.6.1 of R7RS.
      ;
      ; If declaration is not valid, an error is signaled.
      ;
      (unless (and (list? declaration) (not (null? declaration)))
        (error "invalid library declaration" declaration))
        (case (car declaration)
          ((export)
            (expand-export-declaration (cdr declaration) library export-sets))
          ((import)
            (expand-import-declaration (cdr declaration) library export-sets))
          ((begin)
            (expand-begin-declaration (cdr declaration) library))
          ((include-library-declarations)
            (expand-include-library-declarations) (cdr declaration) library)
          (else (error "unknown library declaration" declaration))))

    (define (expand-export-declaration export-specs library export-sets)
      (for-each (lambda (export-spec) (export! library export-spec)) export-specs)
      (empty-sequence))
    
    (define (expand-import-declaration import-sets library export-sets)
      (cond
        ((null? import-sets)
          (error "no import sets in include declaration"))
        (else
          (for-each
            (lambda (import)
              (for-each
                (lambda (binding)
                  (import! library binding))
                (import-set import export-sets)))
            import-sets)
          (empty-sequence))))
          
    (define (expand-begin-declaration body library)
      (unless (list? body)
        (error "invalid begin declaration in library" `(begin . ,body)))
      (make-sequence
        ; XXX When we change make-sequence to accept a list, we have to remove
        ; the apply here.
        (let loop ((body body))
          (if (null? body)
            (list)
            (cons (expand-command-or-definition (car body) library)
              (loop (cdr body)))))))

    (define (expand-include-library-declarations filenames library export-sets)
      (if (null? filenames)
        (error "file name missing in include declaration")
        (make-sequence
          (map
            (lambda (filename)
              (make-sequence
                (map
                  (lambda (declaration)
                    (expand-library-declaration declaration library export-sets))
                  (load-file filename))))
            filenames))))))

