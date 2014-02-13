(define-library (rapid asmjs)
  (export
    emit
    js-function js-return js-var js-block js-statement js-if js-while js-do
    js-for js-break js-continue js-labelled js-switch js-case js-default
    js-expression js-number js-id js-member js-assignment js-conditional
    js-call js-object js-~ js-!
    js-+ js-- js-* js-/ js-% js-or js-& js-^ js-<< js->> js->>> js-< js-<=
    js-> js->= js-== js-!=
    js-module js-signed js-double js-stdlib-import js-math-import
    js-foreign-function js-foreign-int js-foreign-double js-heap-view)
  (import (scheme base))
  (begin

    (define (js-function identifier args . body)
      `(function ,identifier ,args . ,body))
            
    (define (js-return . argument*)
      `(return . ,argument*))
       
    (define (js-var identifier expression)
      `(var ,identifier ,expression))
      
    (define (js-block . body)
      `(block . ,body))
      
    (define (js-statement . expression*)
      `(statement . ,expression*))
      
    (define (js-if test consequent . alternate*)
      `(if ,test ,consequent . ,alternate*))
      
    (define (js-while test . body)
      `(while ,test . ,body))
      
    (define (js-do test . body)
      `(do ,test . ,body))
      
    (define (js-for init* test* update* . body)
      `(for ,init* ,test* ,update* . ,body))
      
    (define (js-break . identifier*)
      `(break . ,identifier*))
      
    (define (js-continue . identifier*)
      `(continue . ,identifier*))
      
    (define (js-labelled identifier statement)
      `(labelled ,identifier ,statement))
      
    (define (js-switch discriminant . clauses)
      `(switch ,discriminant . ,clauses))
      
    (define (js-case test . body)
      `(case ,test . ,body))
      
    (define (js-default . body)
      `(default . ,body))
      
    (define (js-expression . expression*)
      `(expression . ,expression*))
      
    (define (js-number number)
      `(number ,number))
      
    (define (js-string string)
      `(string ,string))
      
    (define (js-id . identifier)
      `(id . ,identifier))
      
    (define (js-new callee . arguments*)
      `(new ,callee . ,arguments*))
      
    (define (js-member object property)
      `(member ,object ,property))
      
    (define (js-assignment identifier expression)
      `(assignment ,identifier ,expression))
      
    (define (js-unary operator expression)
      `(unary ,operator ,expression))
      
    (define (js-binary operator left right)
      `(binary ,operator ,left ,right))
    
    (define (js-conditional test consequent alternate)
      `(condition ,test ,consequent ,alternate))

    (define (js-call identifier . arguments*)
      `(call ,identifier . ,arguments*))

    (define (js-object . property*)
      `(object . ,property*))

    (define (js-module identifier variables functions tables exports)
      (apply js-function identifier '((js-id "stdlib") (js-id "foreign") (js-id "heap"))
        (js-use-asm) `(,@variables ,@functions ,@tables ,exports)))
          
    (define (js-use-asm)
      (js-statement (js-string "use asm")))

    (define (js-signed expression)
      (js-binary "\|" expression (js-number 0)))

    (define (js-double expression)
      (js-unary "+" expression))
    
    (define (js-stdlib identifier)
      (js-member (js-id "stdlib") (js-id identifier)))

    (define (js-math identifier)
      (js-member (js-member (js-id "stdlib") (js-id "Math")) (js-id identifier)))

    (define (js-foreign identifier)
      (js-member (js-id "foreign") (js-id identifier)))

    (define (js-stdlib-import variable identifier)
      (js-var variable (js-stdlib identifier)))
      
    (define (js-math-import variable identifier)
      (js-var variable (js-math identifier)))
      
    (define (js-foreign-function variable identifier)
      (js-var variable (js-foreign identifier)))

    (define (js-foreign-int variable identifier)
      (js-var variable (js-signed (js-foreign identifier))))
      
    (define (js-foreign-double variable identifier)
      (js-var variable (js-double (js-foreign identifier))))

    (define (js-heap-view variable view)
      (js-var variable (js-new (js-stdlib view) (js-id "heap"))))

    (define (js-~ argument)
      (js-unary "~" argument))

    (define (js-! argument)
      (js-unary "!" argument))
    
    (define (make-js-operator operator)
      (letrec ((js-operator
            (lambda args
              (if (null? (cdr args))
                (car args)
                (js-binary operator (apply js-operator (cdr args)) (car args))))))
        js-operator))

    (define js-+ (make-js-operator "+"))
    (define js-- (make-js-operator "-"))
    (define js-* (make-js-operator "*"))
    (define js-/ (make-js-operator "/"))
    (define js-% (make-js-operator "%"))
    (define js-or (make-js-operator "\|"))
    (define js-& (make-js-operator "&"))
    (define js-^ (make-js-operator "^"))
    (define js-<< (make-js-operator "<<"))
    (define js->> (make-js-operator ">>"))
    (define js->>> (make-js-operator ">>>"))
    (define js-< (make-js-operator "<"))
    (define js-<= (make-js-operator "<="))
    (define js-> (make-js-operator ">"))
    (define js->= (make-js-operator ">="))
    (define js-== (make-js-operator "=="))
    (define js-!= (make-js-operator "!="))


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
      (emit ";"))
      
    (define (emit-if test consequent . alternate*)
      (write-string "if(")
      (emit test)
      (write-string "){")
      (emit consequent)
      (write-string "}")
      (unless (null? alternate*)
        (write-string "else{")
        (emit (car alternate*))
        (write-string "}")))
        
    (define (emit-while test . body)
      (write-string "while(")
      (emit test)
      (write-string "){")
      (emit-body body)
      (write-string "}"))
      
    (define (emit-do test . body)
      (write-string "do{")
      (emit-body body)
      (write-string "}while(")
      (emit test)
      (write-string ")"))
      
    (define (emit-for init* test* update* . body)
      (write-string "for(")
      (unless (null? init*)
        (emit (car init*)))
      (write-string ";")
      (unless (null? test*)
        (emit (car test*)))
      (write-string ";")
      (unless (null? update*)
        (emit (car update*)))
      (write-string "){")
      (emit-body body)
      (write-string "}"))

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
      (write-string (number->string number)))
      
    (define (emit-string string)
      (write-string "'")
      (write-string string)
      (write-string "'"))
      
    (define (emit-id identifier)
      (write-string identifier))
      
    (define (emit-new callee . arguments*)
      (emit "new ")
      (emit-parenthesized (not (eq? (car callee) 'member)) callee)
      (emit-args arguments*))
      
    (define (emit-member object property)
      (emit-parenthesized (not (new-expression? object) object))
      (emit object)
      (cond
        ((eq? (car property) 'id)
          (write-string ".")
          (emit property))
        (else
          (write-string "[")
          (emit property)
          (write-string "]"))))

    (define (emit-assignment identifier expression)
      (emit identifier)
      (emit "=")
      (emit-parenthesized (eq? (car expression) 'expression) expression))
    
    (define (emit-unary operator expression)
      (case operator
        (("+" "-") (write-string " ")))
      (write-string operator)
      (emit-parenthesized (not (unary-expression? expression) expression)))
      
    (define (emit-binary operator left right)
      (emit-parenthesized
        (or (not (logical-or-expression? left))  
          (and (eq? (car left) 'binary) (> (precedence (cadr left)) (precedence operator)))) 
        left)
      (case operator
        (("+" "-") (write-string " ")))
      (write-string operator)   
      (emit-parenthesized
        (or (not (logical-or-expression? left))  
          (and (eq? (car left) 'binary) (>= (precedence (cadr right)) (precedence operator)))) 
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
    
    (define (emit-object . property*)
      (string-append "{")
      (let loop ((property* property*) (d ""))
        (unless (null? property*)
          (emit (caar property*))
          (emit ":")
          (emit (cdar property*))
          (loop (cdr property*) ",")))
      (string-append "}"))
    
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
          (emit js)
          (write-string "(")
          (write-string ")"))
        (else (emit js))))

    (define (expression? js)
      (case (car js)
        ((expression number id member assignment unary binary conditional) #t)
        (else #f)))

    (define (new-expression? js)
      (case (car js)
        ((member new) #t)
        (else #f)))

    (define (unary-expression? js)
      (case (car js) 
        ((member new call unary) #t)
        (else #f)))
        
    (define (logical-or-expression? js)
      (case (car js)
        ((member new call unary binary) #t)
        (else #f)))
        
    (define (precedence operator)
      (case operator
        (("*" "/" "%") 5)
        (("+" "-") 6)
        (("<<" ">>" ">>>") 7)
        (("<" ">" "<=" ">=") 8)
        (("==" "!=") 9)
        (("&") 10)
        (("^") 11)
        (("|") 12)))))

