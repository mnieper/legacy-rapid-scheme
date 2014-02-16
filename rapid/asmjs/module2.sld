(define-library (rapid asmjs module)
  (export define-module
    signed double return void foreign stdlib math heap array
    uint8 int8 uint16 int16 uint32 int32 float32 float64
   
;  (rename js-+ +)
;    (rename js-assignment =)
;    (rename 
;    ----> DANN KÖNNEN WIR IN assemble/module.sld +, - verwenden; nachteil: wir können die normalen Scheme-Prozeduren in module nicht mehr verwenden
;    Oder importieren wir module...? Jo... der Rest mit Parametern!       
    )
  (import (scheme base)
  
    (scheme write) ; XXX
   (rapid asmjs))
  (begin


    ;;;
    ;;; Module format
    ;;;
    ;;; 1. Modules
    ;;;
    ;;; (define-module <definition> <name> <global> ... <function> ... <table> ... <exports>)
    ;;; (define-module <definition> <name> <global> ... <function> ... <table> <exports>)
    ;;;
    ;;; Syntax:
    ;;;
    ;;; <definition> has the form <variable> or (<variable> <formals>).
    ;;;
    ;;; <name> is a string.
    ;;;
    ;;; 2. Global
    ;;;
    ;;; (int <identifier>) (int <identifier> <init>)
    ;;; (double <identifier>) (int <identifier> <init>)
    ;;; (stdlib <identifier> <name>)
    ;;; (math <identifier> <name>) 
    ;;; (foreign int <identifier> <name>)
    ;;; (foreign double <identifier> <name>)
    ;;; (foreign <identifier> <name>)
    ;;; (heap <view> <identifier>)
    ;;;
    ;;; 3. Function
    ;;;
    ;;; (function <type> <identifier> (<param> ...) <variable> ... <statement> ... <return>)
    ;;; (function <type> <identifier> (<param> ...) <variable> ... <statement> ...)
    ;;;
    ;;; 4. Table (?)
    ;;;
    ;;; 5. Exports
    ;;;
    ;;; (export <identifier>)
    ;;; (export (<property> <identifier>) ...)
    ;;;
    ;;; 6. Param
    ;;;
    ;;; (int <identifier>)
    ;;; (double <identifier>)
    ;;;
    ;;; 7. Variable
    ;;;
    ;;; (int <identifier>) (int <identifier> init)
    ;;; (double <identifier>) (double <identifier> init)
    ;;;
    ;;; 8. Return
    ;;;
    ;;; (return <identifier>)
    ;;; (return number <number>)
    ;;;
    ;;; 9. Statement
    ;;;
    ;;; (begin ... (macro ... ...)) ; Problem: need type information; what if code generates something?
    ;;; (e.g.: 
    ;;; (js ---
    ;;;
    ;;; Besser: Sprache außerhalb von Modul...
    ;;;
    ;;; Also:
    ;;;
    ;;; (define-module rapidScheme "rapidScheme")
    ;;;
    ;;; (define-global module a)
    ;;; (define-global module b)
    ;;; (define-global module c)
    ;;; 
    
    
    (let (a (genid)) (b (genid)) (c (genid)) (d (genid)) (e (genid))    
    
    (define-syntax return (syntax-rules ()))
    (define-syntax foreign (syntax-rules ()))
    (define-syntax math (syntax-rules ()))
    (define-syntax stdlib (syntax-rules ()))
    (define-syntax heap (syntax-rules ()))
    (define-syntax array (syntax-rules ()))
    (define-syntax void (syntax-rules ()))
    
    (define-syntax var (syntax-rules ()))
    (define-syntax param (syntax-rules ()))
    (define-syntax view (syntax-rules ()))
    
    (define-syntax signed (syntax-rules (var)
        ((_ var) js-signed-var)
        ((_ cast) js-signed)
        ((_ param) js-signed-param)))

    (define-syntax double (syntax-rules (var)
        ((_ var) js-double-var)
        ((_ cast) js-double)
        ((_ param) js-signed-param)))
    
    (define-syntax uint8 (syntax-rules (view)
        ((_ view) "Uint8Array")))
    (define-syntax int8 (syntax-rules (view)
        ((_ view) "Int8Array")))
    (define-syntax uint16 (syntax-rules (view)
        ((_ view) "Uint16Array")))
    (define-syntax int16 (syntax-rules (view)
        ((_ view) "Int16Array")))
    (define-syntax uint32 (syntax-rules (view)
        ((_ view) "Uint32Array")))
    (define-syntax int32 (syntax-rules (view)
        ((_ view) "Int32Array")))
    (define-syntax float32 (syntax-rules (view)
        ((_ view) "Float32Array")))
    (define-syntax float64 (syntax-rules (view)
        ((_ view) "Float64Array")))
    
    ;
    ; TODO Function tables are currently not supported.
    ;

    ;
    ; Export declaration partially missing
    ;
    
    (define-syntax define-module
      (syntax-rules ()
        ((define-module definition name . rest)
          (begin
            (define counter -1)
            (define (genid)
              (set! counter (+ counter 1))
              (js-id (string-append "$" (number->string counter))))
            (define-module-aux "variables" definition name genid `() . rest)))))
            
    (define-syntax define-module-aux
      (syntax-rules (signed double stdlib math foreign heap array quote quasiquote return function)
  
        ((define-module-aux "variables" d n genid `(variable ...) (signed id) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-signed-var id)) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (double id) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-double-var id)) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (stdlib id name) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-stdlib-import id name)) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (math id name) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-math-import id name)) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (foreign signed id name) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-foreign-signed id name)) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (foreign double id name) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-foreign-double id name)) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (foreign id name) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-foreign-function id name)) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (heap type id) . rest)
          (begin
            (define id (genid))
            (define-module-aux "variables" d n genid `(variable ... ,(js-heap-view id (type view))) . rest)))
        ((define-module-aux "variables" d n genid `(variable ...) (array type (id count)) . rest)
          (begin
            (define base (cadr (genid)))
            (define (id i)
              (js-id (string-append base "_" (number->string i))))
            (define-module-aux "variables" d n genid `(variable ... ,@
                (let loop ((i 0))
                  (if (= i count)
                    '()
                    `(,((type var) (id i)) . ,(loop (+ i 1)))))) . rest)))
        ((define-module-aux "variables" d n genid `v* . rest)
          (define-module-aux "functions" d n `v* genid () . rest))
        ((define-module-aux "functions" d n `v* genid f* (function type id param* . body) . rest)  
          (begin
            (define id (genid))
            (define counter -1)
            (define (genlid)
              (set! counter (+ counter 1))
              (js-id (string-append "_" (number->string counter))))
            (define-module-aux "function" (d n `v* genid f* rest) type genlid () (id `()) param* . body)))
        ((define-module-aux "functions" d n `v* genid f* . rest)
          (define-module-aux "export" d n `v* f* . rest))
          
        ((define-module-aux "export" definition name `v* f* (return id))
          (define definition 
            (js-module name `v* (list f*) '() (js-return id))))           ; TODO: Export of object literal
                                   ; Alternative `f* überall
        ((define-module-aux "function" e* t genlid (binding ...) (f `(arg ...) js ...) ((param-type id) . param*) . body)
          (begin
            (define lid (genlid))
            (define-module-aux "function" e* t genlid (binding ... (id lid)) (f `(arg ... ,lid) js ... ((param-type param) lid)) param* . body))) 
        ((define-module-aux "function" e* t genlid b* j* () . body)
          (define-module-aux "declarations" e* t genlid b* j* . body))
        
        ((define-module-aux "declarations" e* t genlid (bindings ...) (js ...) (signed id) . body)
          (begin
            (define lid (genlid))
            (define-module-aux "declarations" e* t genlid (bindings ... (id lid)) (js ... (js-signed-var lid)) . body)))
        ((define-module-aux "declarations" e* t genlid (bindings ...) (js ...) (double id) . body)
          (begin
            (define lid (genlid))
            (define-module-aux "declarations" e* t genlid (bindings ... (id lid)) (js ... (js-signed-var lid)) . body)))
        ((define-module-aux "declarations" e* t g b* j* . body)
          (define-module-aux "body" e* t g b* j* . body))

        ((define-module-aux "body" e* type g b* (js ...) (return argument))
          (define-module-aux "body" e* type g b* (js ... (js-return ((type cast) argument)))))
        ((define-module-aux "body" e* t g b* (js ...) (return))
          (define-module-aux "body" e* t g b* (js ... (js-return))))
        ((define-module-aux "body" e* t g b* (js ...) statement . rest)
          (define-module-aux "body" e* t g b* (js ... statement) . rest))        
        ((define-module-aux "body" (d n `v* genid (fun ...) rest) t g binding* js*)
          (define-module-aux "functions" d n `v* genid (fun ... (let binding* (js-function . js*))) . rest))
          
             
 
            
            ))

    ))
