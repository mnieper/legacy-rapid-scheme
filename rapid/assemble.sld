(define-library (rapid assemble)
  (export assemble)
  (import (scheme base) (scheme cxr) (scheme write) (rapid base))
  (begin  

    (define *constant-true* "0x00010001")    
    (define *constant-false* "0x00000001")
    
    ; FIXME: Integers have to be shifted by 2 for type tags
    ; Internal procedure numberToString has shift back
    ; TODO: numberToString in scheme code
    
    ; TODO: Write small procedures that assemble snippets
    ; TODO: Rename foreign procedures into g1,g2,g3, etc. to save space.

    ; TODO Define library template with define-template macro for compile-time string expansion

    (define (assemble program expression)
    
      (define frame
        (make-parameter 0))
    
      (define body* '())
      
      (define next-label 1)
      
      (define last-global -1)
      
      (define (global-var i)
        (string-append "g" (number->string i)))

      (define (gen-global)
        (set! last-global (+ last-global 1))
        (string-append "g" (number->string last-global)))

      ; with parameterize, this list won't grow too long
      (define variables '())

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
        (write-string expr)) ; FIXME

      (define (assemble-string expr)
        ;
        ; FIXME: Report an error if expr contains null bytes
        ;
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
        ; for set!, we may need this as an lvalue!
        (write-string
          (cond
            ((assq var variables) =>
              (lambda (c)
                (if (string? (cdr c))
                  (cdr c)
                  (let ((d (- (frame) (vector-ref (cdr c) 0))) (i (vector-ref (cdr c) 1)))
                    (let loop ((e "e") (d d))                
                      (if (= d 0)
                        (string-append "h32[(" e "+" (number->string (+ 8 (* i 4))) ")>>2]|0")
                        (loop (string-append "(h32[(" e "+4)>>2]|0)") (- d 1))))))))
            (else
              (error "undefined!")))))
              
      (define (assemble-case-lambda clauses)
        (define label next-label)
        (set! next-label (+ next-label 1))    
        (parameterize (
            (current-output-port (open-output-string))
            (frame (+ (frame) 1)))
          (write-string "case ")
          (write-string (number->string label))
          (write-string ":")
          (write-string "a=h32[e>>2]|0;")
          (let loop ((clauses clauses) (stmt "if"))
            (unless (null? clauses)
              (let ((clause (car clauses)))
                (write-string stmt)
                (assemble-case-lambda-clause (car clause) (cdr clause))
                (loop (cdr clauses) "else if"))))
          (write-string "else{callError();}")
          (set! body* 
            (cons (get-output-string (current-output-port)) body*)))
        (write-string "procedure(")
        (write-string (number->string label))
        (write-string ",e)|0"))

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
                (set! variables (cons (cons (car formals) (vector (frame) i)) variables))
                (loop (cdr formals) (+ i 1))))))
        (write-string "(a|0==")
        (write-string (number->string n))
        (write-string "){")
        (assemble-body body)                
        (write-string "}"))

      (define (assemble-set! var expr)
      
        ; TODO Refactor.
        (write-string
          (cond
            ((assq var variables) =>
              (lambda (c)
                (if (string? (cdr c))
                  (cdr c)
                  (let ((d (- (frame) (vector-ref (cdr c) 0))) (i (vector-ref (cdr c) 1)))
                    (let loop ((e "e") (d d))                
                      (if (= d 0)
                        (string-append "h32[(" e "+" (number->string (+ 8 (* i 4))) ")>>2]")
                        (loop (string-append "(h32[(" e "+4)>>2]|0)") (- d 1))))))))
            (else
              (let ((gv (gen-global)))
                (set! variables (cons (cons var gv) variables))
                gv))))
        (display "=")
        (assemble expr))

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

      ; special handling of ((case-lambda ...) b)? Can directly inline everything!
      (define (assemble-application proc args)
        (write-string "p=frame(")
        (write-string (length args))
        (write-string ")|0;")
        (let loop ((args args) (i 0))
          (unless (null? args)
            (write-string "h32[(p+") (write-string (number->string (+ 8 (* i 4)))) (write-string ")>>2]=")
            (assemble (car args))
            (write-string ";")))
        (write-string "a=")
        (assemble proc)
        (write-string ";")
        (write-string "if((a&0x80000007)>>>0!=0x02){applicationError();}")
        (write-string "a=a&0x7ffffff8;")
        (write-string "h32[(p+4)>>2]=h32[a+4>>2]|0;")
        (write-string "e=p;p=0;")
        (write-string "i=h32[a>>2]|0;")
        (write-string "break"))

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
       
      (define (assemble-code)
        (parameterize ((current-output-port (open-output-string)))
          (write-string "case 0:")
          (assemble expression)
          (write-string ";")
          ; TODO Reverse the order of the following
          (for-each write-string body*)
          (get-output-string (current-output-port))))
      
      (define code (assemble-code))
      
      (define globals
        (parameterize ((current-output-port (open-output-string)))
          (do ((i 0 (+ i 1))) ((> i last-global) (get-output-string (current-output-port)))
            (write-string "var ") (write-string (global-var i)) (write-string "=0;"))))
      
      (parameterize ((current-output-port (open-output-string)))
        (letrec-syntax (
            (write-module (syntax-rules (globals code)
                ((write-module globals element ...)
                  (begin (write-string globals) (write-module element ...)))                    
                ((write-module code element ...)
                  (begin
                    (write-string code)
                    (write-module element ...)))
                ((write-module element1 element2 ...)
                  (begin
                    (write-string element1)
                    (write-module element2 ...)))
                ((write-module)
                  (begin)))))
          (include "rapid/module.scm"))
        (get-output-string (current-output-port))))))

