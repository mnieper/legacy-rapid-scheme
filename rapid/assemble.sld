(define-library (rapid assemble)
  (export assemble)
  (import (scheme base) (scheme cxr) (scheme write) (rapid base))
  (begin  

    (define *constant-true* "0x00010001")    
    (define *constant-false* "0x00000001")
    
    ; Analoguously one may want to define macros for the module... (e.g. IS_PROCEDURE; POINTER(...); etc.)
    ; For use with expand macro
    
    ; FIXME: Integers have to be shifted by 2 for type tags
    ; Internal procedure numberToString has shift back
    ; TODO: numberToString in scheme code
    
    ; TODO: Write small procedures that assemble snippets
    ; TODO: Rename foreign procedures into j1,j2,j3,j4, etc. to save space.

    ; TODO Define library template with define-template macro for compile-time string expansion

    ; FIXME In our current implementation, strings have a fixed number of utf-8 bytes.
    ; This will make problems when implementing "string-set!". Easiest solution: Use utf-16 encoding.
    ; (Or utf-32).
    ;
    ; XXX TODO: Was soll Strings herausgeben? Was soll writes machen? Wie heißen die Prozeduren? Was ist h32[...]?

    (define (assemble program expression)

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

      (define frame (make-parameter 0))
      (define-syntax new-frame
        (syntax-rules ()
          ((new-frame body1 body2 ...)
            (parameterize ((frame (+ (frame) 1)))
              body1 body2 ...))))

      (define (global-var i)
        (write-string "g")
        (write i))
      (define environment '())
      (define last-global -1)
      (define (new-global identifier)
        (set! last-global (+ last-global 1))
        (let* ((global last-global)
               (assemble (lambda (lvalue?) (global-var global))))
          (set! environment (cons (cons identifier assemble) environment))
          (assemble #f)))

      (define (new-local! identifier displacement)
        (let ((f (frame)))
          (set! environment (cons
              (cons identifier
                (lambda (lvalue?)
                  (write-string        
                    (let loop ((e "e") (d (- (frame) f)))    
                      (if (= d 0)
                        (string-append "h32[(" e "+" (number->string (+ 8 (* displacement 4))) ")>>2]")
                        (loop (string-append "(h32[(" e "+4)>>2])|0") (- d 1)))))
                  (unless lvalue? (write-string "|0"))))
            environment))))

      (define (assemble expr)
        (cond
          ((number? expr) (assemble-number expr))
          ((boolean? expr) (assemble-boolean expr))
          ((string? expr) (assemble-string expr))
          ((variable? expr) (assemble-variable expr))
          ((case-lambda? expr) (assemble-case-lambda (cdr expr)))
          ((set!? expr) (assemble-set! (cadr expr) (caddr expr)))
          ((if? expr) (apply assemble-if (cdr expr)))
          ((op expr) => (lambda (op) (assemble-op op (cdr expr))))
          ((pair? expr) (assemble-application (car expr) (cdr expr)))
          (else (error "assemble: unknown expression type" expr))))

      (define (assemble-number expr)
        (write-string (* 2 expr))) ; FIXME

      (define (assemble-string expr)
        ;
        ; work with utf-8
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
          (if expr *constant-true* *constant-false*)))
 
      (define (assemble-variable var)
        ((cdr (assq var environment)) #f))

      (define (assemble-set! var expr)
        (cond
          ((assq var environment) => (lambda (pair) ((cdr pair) #t)))
          (else (new-global var)))
        (write-string "=")
        (assemble expr))

      (define (assemble-case-lambda clauses)
        (define label
          (new-block
             (new-frame
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
                (new-local! (car formals) i)
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
        (display "if((") 
        (assemble pred)
        (display ")>>>0!=0x00000001){")
        (assemble con)
        (display "}else{")
        (assemble alt)
        (display "}"))
      
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
                        (assemble-body (cdar clauses))
                        (outer-loop (cdr clauses))))
                    ((pair? formals)
                      (new-local! (car formals) i)
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
        (do ((i 0 (+ i 1))) ((> i last-global))
          (write-string "var ") (global-var i) (write-string "=0;")))

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
          (include "rapid/module.scm"))))))
