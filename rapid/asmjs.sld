(define-library (rapid asmjs)
  (export
    emit module
    function use-asm variable-declaration variable literal return
    member-expression
    switch while
    signed stdlib math foreign
    import-stdlib import-math)  
  (import (scheme base))
  (begin
  
    (define (module identifier variables functions tables exports)
      (apply function identifier '("stdlib" "foreign" "heap")
        (use-asm) `(,@variables ,@functions ,@tables ,exports)))
  
    (define (function identifier args . body)
      `(function ,identifier ,args . ,body))

    (define (use-asm)
      '(use-asm))

    (define (variable-declaration identifier init)
      `(variable-declaration ,identifier ,init))

    (define (variable identifier)
      `(variable ,identifier))

    (define (while test . body)
      `(while ,test . ,body))

    (define (switch discriminant . cases)
      `(switch ,discriminant . ,cases))

    (define (literal literal)
      `(literal ,literal))

    (define (return argument)
      `(return ,argument))

    (define (member-expression object expression)
      `(member ,object ,expression))

    (define (signed expression)
      `(binary "|" ,expression ,(literal 0)))

    (define (double expression)
      `(unary "+" ,expression))

    (define (stdlib identifier)
      (member-expression (variable "stdlib") identifier))
      
    (define (math identifier)
      (member-expression (member-expression (variable "stdlib") "Math") identifier))

    (define (foreign identifier)
      (member-expression (variable "foreign") identifier))

    (define (import-stdlib variable identifier)
      (variable-declaration (cadr variable) (stdlib identifier)))
      
    (define (import-math variable identifier)
      (variable-declaration (cadr variable) (math identifier)))

    (define (foreign-function variable identifier)
      (variable-declaration (cadr variable) (foreign identifier)))

    (define (foreign-int variable identifier)
      (variable-declaratio (cadr variable) (signed (foreign identifier))))
      
    (define (foreign-double variable identifier)
      (variable-declaration (cadr variable) (double (foreign identifier))))

    (define (emit node)
      (case (car node)
        ((function) (apply emit-function (cdr node)))
        ((variable-declaration) (apply emit-variable-declaration (cdr node)))
        ((use-asm) (emit-use-asm))
        ((variable) (apply emit-variable (cdr node)))
        ((while) (apply emit-while (cdr node)))
        ((switch) (apply emit-switch (cdr node)))
        ((literal) (apply emit-literal (cdr node)))
        ((binary) (apply emit-binary (cdr node)))
        ((unary) (apply emit-unary (cdr node)))
        ((return) (apply emit-return (cdr node)))
        ((member) (apply emit-member (cdr node)))
        (else (error node)))) ; REMOVE ME

    (define (emit-function identifier args . body)
      (write-string "function ")
      (write-string identifier)
      (emit-args args)
      (write-string "{")
      (for-each emit body)
      (write-string "}"))

    (define (emit-use-asm)
      (write-string "'use asm';"))

    (define (emit-variable-declaration identifier init)
      (write-string "var ")
      (write-string identifier)
      (write-string "=")
      (emit init)
      (write-string ";"))

    (define (emit-variable variable) 
      (write-string variable))

    (define (emit-while test . body)
      (write-string "while(")
      (emit test)
      (write-string "){")
      (for-each emit body)
      (write-string "}"))

    (define (emit-switch discriminant . cases)
      (write-string "switch(")
      (emit discriminant)
      (write-string "){")
      (for-each emit cases)
      (write-string "}"))

    (define (emit-literal literal)
      (write-string literal))

    (define (emit-binary operator left right)
      (emit-parenthesized left)
      (write-string operator)
      (emit-parenthesized right))

    (define (emit-unary operator argument) 
      (write-string operator)
      (emit argument))

    (define (emit-return argument)
      (write-string "return ")
      (emit argument)
      (write-string ";"))

    (define (emit-member object expression)
      (emit-parenthesized object)
      (cond
        ((string? expression)
          (write-string ".")
          (write-string expression))
        (else
          (write-string "[")
          (emit expression)
          (write-string "]"))))

    (define (emit-parenthesized expression)
      (case (car expression)
        ((literal variable member) (emit expression))
        (else
          (write-string "(")
          (emit expression)
          (write-string ")"))))

    (define (emit-args args)
      (write-string "(")
      (let loop ((args args) (d ""))
        (unless (null? args)
          (write-string d)
          (write-string (car args))
          (loop (cdr args) ",")))
      (write-string ")"))))
