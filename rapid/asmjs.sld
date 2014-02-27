(define-library (rapid asmjs)
  (export (rename emit emit-js))
  (import (scheme base) (scheme case-lambda))

  ;
  ; TODO Add remaining Javascript constructs.
  ;
  (begin

    (define (emit js)
      (case (car js)
        ((function) (apply emit-function (cdr js)))
        ((return) (apply emit-return (cdr js)))
        ((var) (apply emit-var (cdr js)))
        ((block) (apply emit-block (cdr js)))
        ((statement) (apply emit-statement (cdr js)))
        ((if) (apply emit-if (cdr js)))
        ((while) (apply emit-while (cdr js)))
        ((do) (apply emit-do (cdr js)))
        ((for) (apply emit-for (cdr js)))
        ((break) (apply emit-break (cdr js)))
        ((continue) (apply emit-continue (cdr js)))
        ((labelled) (apply emit-labelled (cdr js)))
        ((switch) (apply emit-switch (cdr js)))
        ((case) (apply emit-case (cdr js)))
        ((default) (apply emit-default (cdr js)))
        ((expression) (apply emit-expression (cdr js)))
        ((number) (apply emit-number (cdr js)))
        ((string) (apply emit-string (cdr js)))
        ((id) (apply emit-id (cdr js)))
        ((new) (apply emit-new (cdr js)))
        ((member) (apply emit-member (cdr js)))
        ((assignment) (apply emit-assignment (cdr js)))
        ((unary) (apply emit-unary (cdr js)))
        ((binary) (apply emit-binary (cdr js)))
        ((conditional) (apply emit-conditional (cdr js)))
        ((call) (apply emit-call (cdr js)))
        ((array) (apply emit-array (cdr js)))
        ((object) (apply emit-object (cdr js)))))

    (define (emit-function identifier args . body)
      (write-string "function")
      (unless (string=? (cadr identifier) "")
        (write-string " ")
        (emit identifier))
      (emit-args args)
      (write-string "{")
      (emit-body body)
      (write-string "}"))
      
    (define (emit-return . argument*)
      (write-string "return")
      (unless (null? argument*)
        (write-string " ")
        (emit (car argument*)))
      (write-string ";"))

    (define (emit-var identifier expression)
      (write-string "var ")
      (emit identifier)
      (write-string "=")
      (emit-parenthesized (eq? (car expression) 'expression) expression)
      (write-string ";"))

    (define (emit-block . body)
      (write-string "{")
      (emit-body body)
      (write-string "}"))
      
    (define (emit-statement . expression*)
      (unless (null? expression*)
        (emit (car expression*)))
      (write-string ";"))

    (define (emit-if test consequent . alternate*)
      (write-string "if(")
      (emit test)
      (write-string ")")
      (cond
        ((and (eq? (car consequent) 'if) (= (length consequent) 3) (not (null? alternate*)))
          (write-string "{")
          (emit consequent)
          (write-string "}"))
        (else (emit consequent)))
      (unless (null? alternate*)
        (write-string " else ")
        (emit (car alternate*))))
        
    (define (emit-while test stmt)
      (write-string "while(")
      (emit test)
      (write-string ")")
      (emit stmt))
      
    (define (emit-do test stmt)
      (write-string "do ")
      (emit stmt)
      (write-string " while(")
      (emit test)
      (write-string ")"))
      
    (define (emit-for init* test* update* stmt)
      (write-string "for(")
      (unless (null? init*)
        (emit (car init*)))
      (write-string ";")
      (unless (null? test*)
        (emit (car test*)))
      (write-string ";")
      (unless (null? update*)
        (emit (car update*)))
      (write-string ")")
      (emit stmt))

    (define (emit-break . identifier*)
      (write-string "break")
      (unless (null? identifier*)
        (write-string " ")
        (emit (car identifier*)))
      (write-string ";"))

    (define (emit-continue . identifier*)
      (write-string "continue")
      (unless (null? identifier*)
        (write-string " ")
        (emit (car identifier*)))
      (write-string ";"))

    (define (emit-labelled identifier statement)
      (emit identifier)
      (write-string ":")
      (emit statement))
      
    (define (emit-switch discriminant . clauses)
      (write-string "switch(")
      (emit discriminant)
      (write-string "){")
      (for-each emit clauses)
      (write-string "}"))
      
    (define (emit-case test . body)
      (write-string "case ")
      (emit test)
      (write-string ":")
      (emit-body body))
      
    (define (emit-default . body)
      (write-string "default:")
      (emit-body body))
      
    (define (emit-expression . expression*)
      (let loop ((expression* expression*) (d ""))
        (unless (null? expression*)
          (write-string d)
          (emit (car expression*))
          (loop (cdr expression*) ","))))
    
    (define (emit-number number)
      (write-string
        (if (and (exact? number) (or (< number -9) (> number 9)))
          (string-append "0x" (number->string number 16))
          (number->string number))))

    (define (emit-string string)
      (write-string "'")
      (write-string string)
      (write-string "'"))

    (define (emit-id identifier . rest)
      (write-string identifier))
      
    (define (emit-new callee . arguments*)
      (write-string "new ")
      (emit-parenthesized (not (eq? (car callee) 'member)) callee)
      (emit-args arguments*))
      
    (define (emit-member object property)
      (emit-parenthesized (not (new-expression? object)) object)
      (cond
        ((string? property)
          (write-string ".")
          (write-string property))
        (else
          (write-string "[")
          (emit property)
          (write-string "]"))))

    (define (emit-assignment identifier expression)
      (emit identifier)
      (write-string "=")
      (emit-parenthesized (eq? (car expression) 'expression) expression))
    
    (define (emit-unary operator expression)
      (when (or (string=? operator "+") (string=? operator "-"))
        (write-string " "))
      (write-string operator)
      (emit-parenthesized (not (unary-expression? expression)) expression))
      
    (define (emit-binary operator left right)
      (emit-parenthesized
        (or (not (logical-or-expression? left))
          (and (eq? (car left) 'binary) (> (precedence (cadr left)) (precedence operator)))) 
        left)
      (when (or (string=? operator "+") (string=? operator "-"))
        (write-string " "))
      (write-string operator)   
      (emit-parenthesized
        (or (not (logical-or-expression? right))  
          (and (eq? (car right) 'binary) (>= (precedence (cadr right)) (precedence operator)))) 
        right))

    (define (emit-conditional test consequent alternate)
      (emit-parenthesized (not (logical-or-expression? test)) test)
      (write-string "?")
      (emit-parenthesized (eq? (car consequent) 'expression) consequent)
      (write-string ":")
      (emit-parenthesized (eq? (car alternate) 'expression) alternate))
    
    (define (emit-call identifier . arguments*)
      (emit identifier)
      (emit-args arguments*))
    
    (define (emit-array . arg*)
      (write-string "[")
      (let loop ((arg* arg*) (d ""))
        (unless (null? arg*)
          (write-string d)
          (emit-parenthesized (eq? (caar arg*) 'expression)
            (car arg*))
          (loop (cdr arg*) ","))) 
      (write-string "]"))
    
    (define (emit-object . property*)
      (write-string "{")
      (let loop ((property* property*) (d ""))
        (unless (null? property*)
          (write-string d)
          (emit (caar property*))
          (write-string ":")
          (emit (cdar property*))
          (loop (cdr property*) ",")))
      (write-string "}"))
    
    (define (emit-body body)
      (unless (null? body)
        (emit (car body))
        (when (expression? (car body))
          (write-string ";"))
        (emit-body (cdr body))))

    (define (emit-args args)
      (write-string "(")
      (let loop ((args args) (d ""))
        (unless (null? args)
          (write-string d)
          (emit-parenthesized (eq? (caar args) 'expression)
            (car args))
          (loop (cdr args) ",")))
      (write-string ")"))

    (define (emit-parenthesized condition js)
      (cond
        (condition
          (write-string "(")
          (emit js)
          (write-string ")"))
        (else (emit js))))

    (define (expression? js)
      (case (car js)
        ((expression number string id member assignment unary binary conditional call) #t)
        (else #f)))

    (define (new-expression? js)
      (case (car js)
        ((number string id member new) #t)
        (else #f)))

    (define (unary-expression? js)
      (case (car js) 
        ((number string id member new call unary) #t)
        (else #f)))
        
    (define (logical-or-expression? js)
      (case (car js)
        ((number string id member new call unary binary) #t)
        (else #f)))
        
    (define (precedence operator)
      (cdr (or (assoc operator *precedences*) (error operator))))

    (define *precedences*
      '(
        ("*" . 5)
        ("/" . 5)
        ("%" . 5)
        ("+" . 6)
        ("-" . 6)
        ("<<" . 7)
        (">>" . 7)
        (">>>" . 7)
        ("<" . 8)
        (">" . 8)
        ("<=" . 8)
        (">=" . 8)
        ("==" . 9)
        ("!=" . 9)
        ("&" . 10)
        ("^" . 11)
        ("\|" . 12)))))
