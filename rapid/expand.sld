(define-library (rapid expand)
  (export expand)
  (import (scheme base) (scheme case-lambda)
    (rapid base)
    (rapid sequence)
    (rapid standard-libraries)
    (rapid load)
    (rapid library)
    (rapid import-set))
  (begin

    (define (expand sources)
      (sequence->body
        (let loop ((sources sources) (export-sets export-sets))
          (let ((source (car sources)) (sources (cdr sources)))
            (if (null? sources)
              (expand-program source export-sets)
              (let-values (((code export-set) (expand-library source export-sets)))
                (make-sequence code (loop sources (cons export-set export-sets)))))))))



    ; TODO (e.g. implement in terms of libraries)
    (define (expand-program source export-sets)
      (make-sequence source))

    (define (expand-library source export-sets)
      (let ((library-name (list-ref source 1)))
        (when (assoc library-name export-sets)
          (error "double defintion of library" library-name))
        (let* ((library (make-library library-name))
               (code
                 (apply make-sequence
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
          ((include-library-declarations)
            (expand-include-library-declarations) (cdr declaration) library export-sets)
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

    (define (expand-include-library-declarations filenames library export-sets)
      (if (null? filenames)
        (error "file name missing in include declaration")
        (apply make-sequence
          (map
            (lambda (filename)
              (apply make-sequence
                (map
                  (lambda (declaration)
                    (expand-library-declaration declaration library export-sets))
                  (load-file filename))))
            filenames))))))
