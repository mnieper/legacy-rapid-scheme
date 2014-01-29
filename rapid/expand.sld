(define-library (rapid expand)
  (export expand)
  (import (scheme base) (scheme case-lambda)
    (rapid standard-libraries) (rapid load) (rapid library) (rapid import-set))
  (begin

    (define (make-sequence . code)
      (cons begin code))
    
    (define-syntax (empty-sequence)
      (make-sequence))
    
    (define (expand sources)
      (let loop ((sources sources) (export-sets export-sets))
        (let ((source (car sources)) (sources (cdr sources)))
          (if (null? sources)
            (expand-program source export-sets)
            (let-values ((code export-set) (expand-library source export-sets))
              (make-sequence code (loop sources (cons export-set exports-sets))))))))

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
            filenames))))

 #|
    ; arguments
    ; xxx: main parameter of expand procedure
    ; sources: sources still to be processed
    ; export-sets: libraries already processed
    ; environment: current lexical environment
    ; imports: bindings currently imported by the library
    ; exports: bindings currently exported by the library
    ; library-name: library that is currently processed

    ; sources is a list of library defintions followed by a list holding the program
    
    ; export-sets is an alist indexed by library names whose cadr's are alists indexed
    ; by the name of the exported identifier
    
    ; exports is an alist indexed by the exported identifiers whose cadr's are
    ; the library identifiers

    ; lexical environment is liste von alists

    (define expand
      (case-lambda
        ((sources)
          (expand sources '()))
        ((sources export-sets)
          (let ((source (car sources)) (sources (cdr sources)))
            (cond
              ((null? sources) (expand-program source export-sets))
              (else (expand-library source sources export-sets)))))))

    (define (expand-library source sources export-sets)
      (let ((library-name (cadr source)))
        (when (assoc library-name export-sets)
          (error "double defintion of library" library-name))
        (expand-library-declarations (cddr source) sources export-sets '(()) '() '() library-name)))
      

    (define (expand-library-declarations
        declarations sources export-sets environment imports exports library-name)
      (cond
        ((null? declarations)
          (expand sources `((,library-name ,(export-set exports environment)) . ,export-sets)))
        (else
          (let ((declaration (car declarations)) (declarations (cdr declarations)))
            (unless (and (list? declaration) (not (null? declaration)))
              (error "invalid library declaration" declaration))
            (case (car declaration)
              ((export)
                (expand-export-declaration (cdr declaration) declarations sources
                  export-sets environment imports exports library-name))
              ((import)
                (expand-import-declaration (cdr declaration) declarations sources
                  export-sets environment imports exports library-name))
              ((include-library-declarations) (cdr declaration) declarations sources
                  export-sets environment imports exports library-name)
              (else (error "unknown library declaration" declaration))
            )))))
  
    (define (expand-export-declaration export-specs declarations sources
        export-sets environment imports exports library-name)
      (let loop ((export-specs export-specs) (exports exports))
        (if (null? export-specs)
          (expand-library-declarations declarations sources export-sets environment imports exports library-name)
          (let export ((export-spec (car export-specs)) (export-specs (cdr export-specs)))
            (cond
              ((symbol? export-spec)
                (export `(rename ,export-spec ,export-spec) export-specs))
              (else
                (unless
                  (and (list? export-spec) (= (length export-spec) 3)
                    (eq? (list-ref export-spec 0) 'rename) (symbol? (list-ref export-spec 1))
                    (symbol? (list-ref export-spec 2)))
                  (error "invalid export specification" export-spec))
                (when (assq (list-ref export-spec 2) exports)  ; TODO `define list-ref export-spec 2
                  (error "identifier exported more than once" (list-ref export-spec 2)))
                (loop (export-specs `(,export-spec . exports)))))))))

    (define (expand-import-declaration import-sets declarations sources
        export-sets environment imports exports library-name)
      (if (null? import-sets)
        (error "no import sets in include declaration")
        (expand-library-declarations
          declarations sources export-sets environment 
            `(
              (let loop ((import-sets import-sets))
                (if (null? import-sets)
                  '()
                  (let ((imports (import-set (car import-sets) export-sets)))
                    ; TODO: Check whether imports are in environment
                    ; Add identifiers to environment (das machen wir einen nach dem nächsten
                    ; oder in routine import-set -> import-sets?
                    `(,imports . (loop (cdr import-sets)))
                  ))
                  )) . ,imports)
            exports library-name)))

    (define (include-library-declarations filenames declarations sources
        export-sets environment imports exports library-name)
      (if (null? filenames)
        (error "file name missing in include declaration")
        (expand-library-declarations
          (let loop ((filenames filenames))
            (define filename (car filenames))
            (define filenames (cdr filenames))
            `(,(load-file filename) .
              ,(if (null? filenames)
                declarations
                (loop (car filenames) filenames))))
          sources export-sets environment imports exports library-name)))
        
    ; bei include:
    ; lade inhalt
    ; füge inhalt an source an


    ; include library declarations als nächstes!

    (define (export-set exports environment)
      (if (null? exports)
        '()
        (let ((export (car exports)) (exports (cdr exports)))
          (define identifier1 (list-ref export 1))
          (define identifier2 (list-ref export 2))
          (cond
            ((assq identifier1 (car environment)) =>
              (lambda (binding)
                `((,identifier2 ,(cadr binding)) . (export-set exports environment))))
            (else
              (error "cannot export unbound identifier" identifier1))))))


#|
(define (compile-library export-sets source)
  (let ((library-name (library-name source)))
    (when (assoc library-name export-sets)
      (error "double definition of library" library-name))
    (let loop ((declarations (cddr source)) (export-specs '()) (imports '()) (environment '()))
      (if (null? declarations)
        ; map exported identifiers
        ; check whether an identifier is exported twice
        ; check whether an identifier has two bindings in imports
        ; check whether an identifier bound in imports is being set!
        `(,library-name ,(exports export-specs environment))
        (let ((declaration (car declarations)) (declarations (cdr declarations)))      
          (unless (and (list? declaration) (not (null? declaration)))
            (error "invalid library declaration" declaration))
          (case (car declaration)
            ((export) 
              (loop declarations (append export-specs (cdr declarations)) imports environment))
            ((import)
              (loop declarations export-specs
                ; imports sollen auch ans environment!
                (apply append imports (map (lambda (import) (import-set import export-sets)) (cdr declarations)))
                environment))
            ((begin) (error "not implemented" declaration))
            ((include) (error "not implemented" declaration))
            ((include-ci) (error "not implemented" declaration))
            ((include-library-declarations) (error "not implemented" declaration))
            ((cond-expand) (error "not implemented" declaration))
            (else (error "unknown library declaration" declaration))))))))
|#

|#

