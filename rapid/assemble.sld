;;
;; Machine model
;;
;; Small values that should fit into a scalar:
;; boolean? char? null? pair? eof-object? fixnum? double? exact-integer?
;; cells on the heap are 8 bytes long (for what?)
;; for environments, etc: reserve 8 bits
;;
;; The most complicated type seems to be the pair.
;; It consists of a scalar value and a pointer
;;
;; what is a float? a pointer to two cells
;;
;; On the heap, the gc algorithm must know how long an object is.
;; By default, an object is exactly 64 bits long.
;; longer objects (strings, vectors, etc. have their first byte marked)
;;
;;
;; 

(define-library (rapid assemble)
  (export assemble)
  (import (scheme base) (scheme cxr) (scheme write) (rapid base))
  (begin  

    (define (assemble program expression)
    
      (define frame
        (make-parameter 0))
    
      (define body* '())
      
      (define next-label 1)

      (define gensym (make-gensym "$"))

      (define (genvar)
        (symbol->string (gensym)))

      ; with parameterize, this list does not grow too long
      (define variables '())

      ; should also be local
      (define global-counter -1)
      (define (gen-global-var)
        (set! global-counter (+ global-counter 1))
        (indexed-var "$" global-counter)) 

      (define (indexed-var array-var index)
        ;
        ; TODO: Use at more places.
        ;
        (string-append array-var "["
          (number->string index) "]"))

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
        (write expr)) ; FIXME

      (define *constant-true* "0x0001000a")
      
      (define *constant-false* "0x0000000a")

      (define (assemble-boolean expr)
        (write-string
          (if expr *constant-true* *constant-false*)))
      
      (define (assemble-variable var)
        ; for set!, we may need this as an lvalue!
        (write-string
          (cond
            ((assq var variables) =>
              (lambda vec)
                (let ((d (- frame (vector-ref vec 0))) (i (vector-ref vec 1)))
                  (let loop ((e "e") (d d))                
                    (if (= d 0)
                      (string-append "heap[(" e "+8+4*i)>>2]|0")
                      (loop (string-append "heap[(" e "+4)>>2]|0") (- d 1))))))
            (else
              (error "not implemented yet")
              (let ((gv (gen-global-var)))
                (set! variables (cons (cons var gv) variables))
                gv)))))
                
      (define (assemble-case-lambda clauses)
        (define label next-label)
        (set! next-label (+ next-label 1))    
        (parameterize (
            (current-output-port (open-output-string))
            (frame (+ (frame) 1)))
          (write-string "case ")
          (write-string (number->string label))
          (write-string ":")
          (write-string "a=heap[e>>2]|0;")
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
        (write-string ")"))

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
                (set! variables (cons (cons (car formals) (vector (frame) i)) variables))))))
        (write-string "(n==")
        (write-string (number->string n))
        (write-string "){")
        (assemble-body body)                
        (write-string "}"))

      (define (assemble-set! var expr)
        (assemble var)
        (display "=")
        (assemble expr))

      (define (assemble-if pred con alt)
        (display "if(") 
        (assemble pred)
        (display ".toBoolean()){")
        (assemble con)
        (display "}else{")
        (assemble alt)
        (display "}"))
      
      (define (assemble-op op args)
        (write-string op)
        (write-string "(")
        (assemble-args args)
        (write-string ")"))

      (define (assemble-application proc args)
        (write-string "p=frame(")
        (write-string (length args))
        (write-string ");")
        (let loop ((args args) (i 0))
          (unless (null? args)
            (write-string "h32[(p+8+4*i)>>2]=")
            (assemble (car args))
            (write-string ";")))
        (write-string "a=")
        (assemble proc)
        (write-string ";")
        (write-string "h32[(p+4)>>2]=h32[(a&0x7ffffff8+8)>>2]|0;")  ; FIXME: new procedure format (just 8 bytes)
        (write-string "e=p;p=0;")
        (write-string "i=h32[(a&0x7ffffff8+4)>>2]|0;")
        (write-string "break;"))

      (define (assemble-body body)
        (unless (null? body)
          (assemble (car body))
          (unless (null? (cdr body))
            (write-string ";"))
          (assemble-body (cdr body))))
                
      (define (assemble-args args)
        (unless (null? args)
          (assemble (car args))
          (let ((rest (cdr args)))
            (unless (null? rest)
              (write-string ",")
              (assemble-args rest)))))
       
      (define (assemble-code)
        (write-string "case 0:")
        (assemble expression)
        (for-each write-string body))
      
      (parameterize ((current-output-port (open-output-string)))
        (letrec-syntax (
            (write-module (syntax-rules (code)
                ((write-module code element ...)
                    (begin
                      (assemble-code)
                      (write-module element ...)))
                ((write-module element1 element2 ...)
                    (begin
                      (write-string element1)
                      (write-module element2 ...)))
                ((write-module)
                    (begin)))))
          (include "rapid/module.scm"))
        (get-output-string (current-output-port))))))

