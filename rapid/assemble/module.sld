(define-library (rapid assemble module)
  (export module
    ;aux-reg code-reg free-ptr env-ptr frame-ptr string-ptr global
  )
  (import (rapid asmjs module) (rapid asmjs))
  (begin
  
    (define-module (module global-count)
      "RapidModule"
      (signed aux-reg)
      (signed code-reg)
      (signed free-ptr)
      (signed env-ptr)
      (signed frame-ptr)
      (signed string-ptr)
      (array signed (global global-count))
      (math imul "imul")
      (foreign signed heap-size "heapSize")
      (foreign exit "exit")
      (foreign write-string "writeString")
      (foreign memory-error "memoryError")
      (foreign call-error "callError")
      (foreign application-error "applicationError")
      (heap int32 h32)
      (heap uint8 hu8)
      
      (function signed run ((signed s))
        (double p)
        
        
        (return p))
      
      
      (return run)) 
  
  #|
    (define-module (module global-count)
      "RapidModule"
      (signed aux-reg)
      (signed code-reg)
      (signed free-ptr)
      (signed env-ptr)
      (signed frame-ptr)
      (signed string-ptr)
      (array signed (global global-count))
      (math imul "imul")
      (foreign signed heap-size "heapSize")
      (foreign exit "exit")
      (foreign write-string "writeString")
      (foreign memory-error "memoryError")
      (foreign call-error "callError")
      (foreign application-error "applicationError")
      (heap int32 h32)
      (heap uint8 hu8)
      
      
      (function signed alloc ((signed s))
        (signed p)
        (js-assignment p free-ptr)
        (js-assignment free-ptr (js-signed (js-+ free-ptr s)))
        (js-if (js-> (js-signed free-ptr) heap-size) (js-call memory-error))
        (return p)))
|#
      
      
      ))
      
      
