(define-library (rapid compile)
  (export compile)
  (import
    (scheme base)
    (scheme file)
    (scheme write)
    (chibi show)
    (chibi show pretty)
    (rapid base)
    (rapid program)
    (rapid load)
    (rapid link)
    (rapid cps)
    (rapid optimize)
    (rapid codegen)
    (rapid assemble)
    (rapid output))
  (begin

    ; XXX Put output, etc. into this file; Put this into compiler.scm/.sld

    ; Check whether environment monad is interesting for us

    (define (log string obj)
      (with-output-to-file string (lambda ()
          (show #t (pretty obj))
          obj)))

    (define (compile)
      (define program (make-program))
      
      (write-string "rapid-scheme 0.1\n" (current-error-port))
      (write-string "Copyright © 2014 Marc Nieper-Wißkirchen\n" (current-error-port))
      
      (let* ((code (log "read.out" (read-file)))      ; -> read
          (code (log "preprocess.out" (link code)))           ; -> preprocess
          (code (log "compile.out" (cps program code)))    ; -> compile
          (code (log "optimize.out" (optimize code)))       ; -> optimize
          (code (log "codegen.out" (codegen code)))      ; -> codegen
          (code (log "assemble.out" (assemble-module code)))         ; -> assemble
          (code (log "output.log" (output code))))    ;  -> output
          code))))
                 
         
