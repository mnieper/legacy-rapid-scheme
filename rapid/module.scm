(module "RapidScheme"
  (signed aux-reg)
  (signed code-reg)
  (signed env-ptr)
  (signed frame-ptr)
  (foreign signed heap-size "heapSize")
  (foreign function void call-error "callError")
  (foreign function void application-error "applicationError")
  (foreign function void memory-error "memoryError")
  (foreign function void write-string "writeString")
  (heap i32 "Int32Array")

  (function signed alloc ((signed s))
    (signed p)
    (set! p frame-ptr)
    (set! frame-ptr (+ frame-ptr s))
    (if (> frame-ptr heap-size)
      (memory-error))
    (return p))
  
  (function void run ()
    (while 1
      (switch code-reg)))
      
  (return run))

