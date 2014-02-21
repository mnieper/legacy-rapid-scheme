(module "RapidScheme"
  (signed aux-reg)
  (signed code-reg)
  (signed data-ptr)
  (signed env-ptr)
  (signed frame-ptr)
  (math imul "imul")
  (heap i32 "Int32Array")
  (foreign signed heap-size "heapSize")
  (foreign function void exit "exit")  
  (foreign function void call-error "callError")
  (foreign function void application-error "applicationError")
  (foreign function void memory-error "memoryError")
  (foreign function void write-string "writeString")
  ,@globals

  (function signed alloc ((signed s))
    (signed p)
    (set! p frame-ptr)
    (set! frame-ptr (+ frame-ptr s))
    (if (> frame-ptr heap-size)
      (memory-error))
    (return p))
  
  (function void run ()
    
    (while 1
      (switch code-reg
        ,@blocks)))
    

  (return run))

