(module "RapidScheme"
  (signed aux-reg)
  (signed code-reg)
  (signed data-ptr)
  (signed env-ptr)
  (signed frame-ptr)
  (signed free-ptr)
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
    (set! p free-ptr)
    (set! free-ptr (+ free-ptr s))
    (if (> free-ptr heap-size)
      (memory-error))
    (return p))

  (function signed frame ((signed arg-count))
    (signed p)
    (set! p (alloc (and (* 4 (+ arg-count 3)) #xfffffff8)))
    (set! ,(at 'p) arg-count)
    (return p))

  (function signed procedure ((signed label) (signed parent-frame-ptr))
    (signed p)
    (set! p (alloc 8))
    (set! ,(at 'p) label)
    (set! ,(at '(+ p 4)) parent-frame-ptr)
    (set! p (or p ,*proc-tag*))
    (return p))
    
  (function signed equality ((signed i1) (signed i2))
    (return (if (= i1 i2) ,true ,false)))

  (function signed sum ((signed i1) (signed i2))
    ;
    ; FIXME Allow multiple arguments.
    ; FIXME Allow any number.
    ; FIXME Check for overflow.
    ;
    (return (+ i1 i2)))

  (function signed difference ((signed i1) (signed i2))
    (return (+ i1 (- i2))))

  (function signed product ((signed i1) (signed i2))
    (return ,(box-i32 `(imul ,(unbox-i32 'i1) ,(unbox-i32 'i2))))) 

  (function signed increasing? ((signed i1) (signed i2))
    (return (if (< i1 i2) ,true ,false)))
    
  (function signed truncate-remainder ((signed i1) (signed i2))
    (return ,(box-i32 `(remainder ,(unbox-i32 'i1) ,(unbox-i32 'i2)))))

  (function signed number->string ((signed i))
    (signed n)
    (signed j)
    (signed d)
    (signed m)
    (signed s)
    (set! i ,(unbox-i32 'i))
    (if (< i 0)
      (begin
        (set! i (- i))
        (set! d 1)
        (set! n (+ n 1)))
      (if (= i 0)
        (set! n 1)
        (begin
          (for ((set! j i)) ((not= j 0)) ((= j (/ j 10)))
            (set! n (+ n 1))))))
    (set! s (alloc (+ (* 4 n) 8)))
    (set! ,(at 's) ,*string-tag*)
    (set! ,(at '(+ s 4)) (* 4 n))
    (if (= d 1)
      (set! ,(at '(+ s 8)) ,(char->integer #\-)))
    (for ((set! j (+ n -1))) ((> j 0)) ((set! j (+ j -1)))
      (set! m (remainder i 10))
      (set! ,(at '(+ s 8 (* 4 j))) (+ m ,(char->integer #\0)))
      (set! i (/ (+ i (- m)) 10)))
    (return s))  
  
  (function void run ()
    
    (while 1
      (switch code-reg
        ,@blocks)))
    
  (return run))

