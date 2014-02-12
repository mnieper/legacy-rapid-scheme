(define-library (rapid assemble)
  (export assemble)
  (import
    (scheme base)
    (scheme write) ; XXX Get rid of this; use string->number
    (scheme case-lambda)
    (only (rapid base) output-from)
    (rapid scheme)
    (rapid asmjs))
  (begin

    ; Naming: environment extracts information about the variables
    ;         translate translates an expression and accumulates a body

    (define (make-location assemble)
      (vector assemble))

    (define (location-assemble location)
      (vector-ref location 0))

    (define (make-global counter)
      (let ((name (global-reg counter)))
        (make-location
          (lambda (frames value?) name)))) 

    (define (make-local frame displacement)
      (make-location
        (lambda (frames value?)
          (let ((depth (list-ref frames frame)))
            (if value?
              (local-value depth displacement)
              (local-location depth displacement))))))

    ; TODO: Find a better name for environment
    (define environment
      (case-lambda
        ((expression) (environment expression 0 0 '()))
        ((expression frame global-count locations)
          (cond
            ((case-lambda? expression) (environment-case-lambda (case-lambda-clauses expression) frame global-count locations))
            ((set!? expression) (environment-set! (set!-variable expression) (set!-expression expression) frame global-count locations))
            ((if? expression) (environment* (cdr expression) frame global-count locations))
            ((op expression) (environment* (cdr expression) frame global-count locations))
            ((pair? expression) (environment* expression frame global-count locations))
            (else (values global-count locations))))))

    (define (environment* expressions frame global-count locations)
      (if (null? expressions)
        (values global-count locations)
        (let-values (((global-count locations)
                      (environment (car expressions) frame global-count locations)))
          (environment* (cdr expressions) frame global-count locations))))

    (define (environment-set! variable expression frame global-count locations)
      (if (assq variable locations)
        (environment expression frame global-count locations)
        (environment expression frame (+ global-count 1)
          `((,variable . ,(make-global global-count)) . ,locations))))

    (define (environment-case-lambda clauses frame global-count locations)
      (let ((frame (+ frame 1)))
        (let loop ((clauses clauses))
          (if (null? clauses)
            (values global-count locations)
            (let-values (((global-count locations) (loop (cdr clauses))))
              (let ((clause (car clauses)))
                (environment* (case-lambda-clause-body clause) frame global-count              
                  (let loop-formals ((formals (case-lambda-clause-formals clause)) (i 0))
                    (cond
                      ((symbol? formals) `((,formals . ,(make-local frame i)) . ,locations))
                      ((null? formals) locations)
                      ((pair? formals) `((,(car formals) . ,(make-local frame i)) . ,(loop-formals (cdr formals) (+ i 1)))))))))))))   



    ; Analoguously one may want to define macros for the module... (e.g. IS_PROCEDURE; POINTER(...); etc.)
    ; For use with expand macro
    
    ; TODO: Rename foreign procedures into j1,j2,j3,j4, etc. to save space.

    ; TODO Define library template with define-template macro for compile-time string expansion

    ; FIXME In our current implementation, strings have a fixed number of utf-8 bytes.
    ; (solution: utf-32).
  
    (define translate
      (case-lambda
        ((expression locations) (body expression '(0) 1 locations))
        ((expression frame-depths next-label locations)
          (cond
            ((number? expression) (translate-number expression))
            ((boolean? expression) (translate-boolean expression))
            ((string? expression) (translate-string expression))
            ((variable? expression) (translate-variable expression frame-depths locations))
            ((set!? expression) (translate-set! (set!-variable expression) (set!-expression expression) frame-depths next-label locations))
            ((op expression) => (lambda (op) (translate-operation op (cdr expression) frame-depths next-label locations)))
            ((if? expression) (translate-if (if-test expression) (if-consequent expression) (if-alternate expression)))
            ((case-lambda? expression) (assemble-case-lambda (cdr expr)))
            ((pair? expression) (assemble-application (car expr) (cdr expr)))))))

    (define (translate* expression* frame-depths next-label locations)
      (if (null? expression*)
        (values '() (empty-body))
        (let-values (((code* assembled-body) (translate* (cdr expression*) frame-depths next-label locations))
            ((code body) (translate (car expression*) frame-depths next-label locations)))
          (values `(,code . ,code*) (string-append body assembled-body))))) ; TODO move string-append to asmjs

    (define (translate-number expression)
      (values
        (write-string (number expression)) (empty-body)))

    (define (translate-boolean expression)
      (values
        (write-string (boolean expression)) (empty-body)))

    (define (translate-string expression)
      (values
        (write-string (string expression) (empty-body))))

    (define (translate-variable variable frame-depths locations)
      (values
        (let ((location (cdr (assq variable locations))))
          ((location-assemble location) frame-depths #t))
        (empty-body)))

    (define (translate-set! variable expression frame-depths next-label locations)
      (let ((location (cdr (assq var locations))))
        (let-values (((code body) (translate expression frame-depths next-label locations)))
          (values
            (assignment ((location-assemble location) (frames) #f) code)
            body))))
      
    (define (translate-operation operation args frame-depths next-label locations)
      (let-values (((code* body)) (translate* args frame-depths next-label locations))
        (values (call operation code*) body)))

    (define (translate-if test consequent alternate frame-depths next-label locations)
      (let-values (((code* body) (translate* (list test consequent alternate) frame-depths next-label locations)))
        (values (apply conditional code*) body)))

    (define (assemble expression program)
      (let-values (((global-count locations)
                    (environment expression)))

        ; wie funktioniert das neue Assemble:
        ; Erzeuge blocks; außerdem sollte frames weitergeben werden; frames ist das neue depths
        ;   

        ; mitzuschleppen: environment, frame-depths, block-label?
        

                
                
            
            
        (define block* '())
        (define block-label -1)
        (define-syntax new-block
          (syntax-rules ()
            ((new-block body1 body2 ...)
              (begin
                (set! block-label (+ block-label 1))
                (let* ((block-label block-label)
                       (block       
                         (output-from
                           (write-string "case ")
                           (write block-label)
                           (write-string ":")
                           (let ()
                             body1 body2 ...))))
                  (set! block* (cons block block*))
                  block-label)))))

        (define frames (make-parameter '(0)))
        (define-syntax new-frame
          (syntax-rules ()
            ((new-frame increase body1 body2 ...)
              (parameterize (
                  (frames (let loop ((frames (frames)))
                            (if (null? frames)
                              '(0)
                              `(,(+ (car frames) increase) . ,(loop (cdr frames)))))))
                body1 body2 ...))))

        (define (assemble expr)
          (cond
            ((number? expr) (assemble-number expr))
            ((boolean? expr) (assemble-boolean expr))
            ((string? expr) (assemble-string expr))
            ((variable? expr) (assemble-variable expr))
            ((case-lambda? expr) (assemble-case-lambda (cdr expr)))
            ((set!? expr) (assemble-set! (set!-variable expr) (set!-expression expr)))
            ((if? expr) (apply assemble-if (cdr expr)))
            ((op expr) => (lambda (op) (assemble-op op (cdr expr))))
            ((pair? expr) (assemble-application (car expr) (cdr expr)))
            (else (error "assemble: unknown expression type" expr))))

        (define (assemble-number expr)
          (write-string (* 2 expr))) ; FIXME

        (define (assemble-string expr)
          ;
          ; work with utf-32
          ; TODO: Simplify the following
          ;
          (define utf8 (string->utf8 expr))
          (write-string "(s=alloc(")
          (write-string (number->string (* 8 (quotient (+ 16 (bytevector-length utf8)) 8))))
          (write-string ")|0,h32[s>>2]=0x0,h32[s+4>>2]=")
          (write-string (number->string (bytevector-length utf8)))
          (do ((i 0 (+ i 1))) ((= i (bytevector-length utf8)))
            (write-string ",hu8[s+")
            (write-string (number->string (+ i 8)))
            (write-string "|0]=0x")
            (write-string (number->string (bytevector-u8-ref utf8 i) 16)))
          (write-string ",hu8[s+") (write-string (number->string (+ 8 (bytevector-length utf8)))) (write-string "|0]=0x0")
          (write-string ",s)|0"))

        (define (assemble-boolean expr)
          (write-string
            (boolean expr)))
   
        ; TODO: We may not want to use the parameter object frame...
        (define (assemble-variable var)
          (let ((location (cdr (assq var locations))))
            ((location-assemble location) (frames) #t)))

        ; Here: as well!
        (define (assemble-set! var expr)
          (let ((location (cdr (assq var locations))))
            ((location-assemble location) (frames) #f))
          (write-string "=")
          (assemble expr))

        (define (assemble-case-lambda clauses)
          (define label
            (new-block
               (new-frame 1
                 (write-string "a=h32[e>>2]|0;")
                 (assemble-case-lambda-body clauses))))
          (write-string "procedure(")
          (write-string (number->string label))
          (write-string ",e)|0"))

        (define (assemble-case-lambda-body clauses)          
            (let loop ((clauses clauses) (stmt "if"))
              (unless (null? clauses)
                (let ((clause (car clauses)))
                  (write-string stmt)
                  (assemble-case-lambda-clause (car clause) (cdr clause))
                  (loop (cdr clauses) "else if"))))
            (write-string "else{callError();}"))

        (define (assemble-case-lambda-clause formals body)
          ; FIXME At the moment this does not work with rest arguments because lists are not yet implemented
          (define n
            (let loop ((formals formals) (i 0))
              (cond
                ((symbol? formals)
                  (error "not yet implemented, see above"))
                ((null? formals)
                  i)
                ((pair? formals)
                  (loop (cdr formals) (+ i 1))))))
          (write-string "((a|0)==")
          (write-string (number->string n))
          (write-string "){")
          (assemble-body body)                
          (write-string "}"))


        (define (assemble-if pred con alt)
          ;
          ; TODO: There are no semicola in the branches, but one at the end!
          ;
          (write-string "if((") 
          (assemble pred)
          (write-string ")>>>0!=0x00000001){")
          (assemble con)
          (write-string "}else{")
          (assemble alt)
          (write-string "}"))
        
        (define (assemble-op op args)
          (write-string op)
          (write-string "(")
          (assemble-args args)
          (write-string ")|0"))

        (define (assemble-application proc args)
          (define n (length args))
          (write-string "p=frame(")
          (write-string (length args))
          (write-string ")|0;")
          (let loop ((args args) (i 0))
            (unless (null? args)
              (write-string "h32[(p+") (write-string (number->string (+ 8 (* i 4)))) (write-string ")>>2]=")
              (assemble (car args))
              (write-string ";")
              (loop (cdr args) (+ i 1))))
          (cond
            ((case-lambda? proc)
              (write-string "e=p;p=0;")
              (let outer-loop ((clauses (cdr proc)))
                (if (null? clauses)
                  (error "procedure called with wrong number of arguments") ; FIXME
                  (let loop ((formals (caar clauses)) (i 0))
                    (cond
                      ((symbol? formals)
                        (error "not yet implemented, see above"))
                      ((null? formals)
                        (if (= i n)
                          (new-frame 0
                            (assemble-body (cdar clauses)))
                          (outer-loop (cdr clauses))))
                      ((pair? formals)
                        (loop (cdr formals) (+ i 1))))))))
            (else
              (write-string "a=")
              (assemble proc)
              (write-string ";")
              (write-string "if((a&0x80000007)>>>0!=0x3){applicationError();}")
              (write-string "a=a&0x7ffffff8;")
              (write-string "h32[(p+4)>>2]=h32[a+4>>2]|0;")
              (write-string "e=p;p=0;")
              (write-string "i=h32[a>>2]|0;")
              (write-string "break"))))

        (define (assemble-body body)
          (unless (null? body)
            (assemble (car body))
            ;(unless (null? (cdr body))
            (write-string ";")
            (assemble-body (cdr body))))

        (define (assemble-args args)
          (unless (null? args)
            (assemble (car args))
            (let ((rest (cdr args)))
              (unless (null? rest)
                (write-string ",")
                (assemble-args rest)))))
         
        (new-block
          ; ENTER ASM.JS INIT CODE HERE
          (assemble expression)
          (write-string ";"))

        (define (globals)
          (do ((i 0 (+ i 1))) ((= i global-count))
            (write-string "var ") (write-string (global-reg i)) (write-string "=0;")))

        (output-from
          (letrec-syntax (
              (write-module (syntax-rules (globals code)
                  ((write-module globals element ...)
                    (begin (globals) (write-module element ...)))                    
                  ((write-module code element ...)
                    (begin
                      (for-each write-string block*)
                      (write-module element ...)))
                  ((write-module element1 element2 ...)
                    (begin
                      (write-string element1)
                      (write-module element2 ...)))
                  ((write-module)
                    (begin)))))
            (include "rapid/module.scm")))))))
