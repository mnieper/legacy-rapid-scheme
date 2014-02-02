(import
  (scheme base)
  (scheme write)
  (rapid error)
  (rapid load)
  (rapid expand)
  (rapid import-set)
  (rapid compile)
  (rapid library))




; TODO: The compilation is completely missing  
(handle-compile-error
  (display
    (expand (load)))
  (newline))

#|
#;(let ((library-table (make-library-table)))
  (let loop ((sources (load)))
    (let ((source (car sources)) (sources (cdr sources)))
      (cond
        ((null? sources)
          (compile-program library-table source))
        (else
          (compile-library library-table source)
          (loop sources))))))
|#
   
; 
; Load libraries in the order they are used

;(with-new-environment
;  (lambda ()
;    (compile (load))))

