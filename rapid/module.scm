(module "RapidModule"
  (signed aux-reg)
  (signed code-reg)
  (signed data-ptr)
  (signed env-ptr ,*null-pointer*)
  (signed frame-ptr ,*null-pointer*)
  (signed free-ptr)
  (signed to-space)
  (signed from-space)
  (signed extent)
  (signed top)
  (signed free)
  (math imul "imul")
  (heap i32 "Int32Array")
  (foreign signed heap-size "heapSize")
  (foreign function void exit "exit")  
  (foreign function void call-error "callError")
  (foreign function void application-error "applicationError")
  (foreign function void memory-error "memoryError")
  (foreign function void write-string "writeString")

  (function signed new ((signed tag) (signed words))
    (signed p)
    (signed s)
    (set! s (<< words 2)) 
    (set! p (allocate s))
    (if (= p ,*null-pointer*)
      (begin
        (collect)
        (set! p (allocate s))
        (if (= p ,*null-pointer*)
          (memory-error))))
    (set! ,(tag 'p) (or tag words))
    (return p))

  (function signed allocate ((signed s))
    (signed result)
    (signed new-free)
    (set! result free)
    (set! new-free (+ free s))
    (if (> new-free top)
      (return ,*null-pointer*))
    (set! free new-free)
    (return ,(pointer->value 'result)))

  ; wir müssen refs finden; diese hängen vom typ des Objektes ab;
  ; bei env-ptr und frame-ptr scannen wir die Argumente... und dann?
  ; Angenommen env-ptr zeigt auf cons; dann landet cons auf dem to-space
  ; in diesem Moment sollten wir die Objekte, auf die cons zeigt, weiter
  ; berücksichtigen, sonst wissen wir nicht, was cons ist.
  ; wichtig im wesentlichen bei float...
  ; wie?

  ; FIXME: The global registers may also contain pointers
  (function void collect ()
    (signed scanned)
    (signed a)
    (return)
    (set! a to-space)
    (set! to-space from-space)
    (set! from-space a)
    (set! free to-space)
    (set! scanned free)
    ;(scan-frame env-ptr)
    ;(if (not= frame-ptr -1)
    ;  (scan-frame frame-ptr))
    #;(while (< scanned free)
      (set! a scanned)
      (set! scanned (+ scanned (size a)))
      (process a)))

  (function signed call ((signed proc) (signed arg))
    (set! env-ptr (new-frame 1))
    (set! ,(arg 'env-ptr 0) arg)
    (if (not ,(proc-value? 'proc)) (application-error))
    (set! ,(frame-parent-frame 'env-ptr) ,(proc-frame 'proc))
    (set! code-reg ,(proc-label 'proc))
    (run))

  (function signed new-frame ((signed arg-count))
    (signed p)
    (signed i)
    (set! p (new ,*frame-tag* ,(frame-size 'arg-count)))
    (set! ,(frame-parent-frame 'p) ,*null-pointer*)
    (for () ((< i arg-count)) ((set! i (+ i 1)))
      (set! ,(arg 'p 'i) ,*null-pointer*))
    (return p))

  (function signed new-proc ((signed label) (signed frame))
    (signed p)
    (set! p (new ,*proc-tag* ,proc-size))
    (set! ,(tag 'p) ,*proc-tag*)
    (set! ,(proc-label 'p) label)
    (set! ,(proc-frame 'p) frame)
    (return p))

  (function signed equality ((signed i1) (signed i2))
    (return (if (= i1 i2) ,true-value ,false-value)))

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
    (return ,(small->value `(imul ,(value->small 'i1) ,(value->small 'i2))))) 

  (function signed increasing? ((signed i1) (signed i2))
    (return (if (< i1 i2) ,true-value ,false-value)))
    
  (function signed truncate-remainder ((signed i1) (signed i2))
    (return ,(small->value `(remainder ,(value->small 'i1) ,(value->small 'i2)))))

  (function signed number->string ((signed i))
    (signed n)
    (signed j)
    (signed d)
    (signed m)
    (signed s)
    (set! i ,(value->small 'i))
    (if (< i 0)
      (begin
        (set! i (- i))
        (set! d 1)
        (set! n (+ n 1))) ; FIXME: Does not handle negative numbers correctly.
      (if (= i 0)
        (set! n 1)
        (begin
          (for ((set! j i)) ((not= j 0)) ((set! j (/ j 10)))
            (set! n (+ n 1))))))
    (set! s (new ,*string-tag* (+ n ,string-header-size)))
    (if (= d 1)
      (set! ,(at '(+ s 4)) ,(char->integer #\-)))
    (for ((set! j (+ n -1))) ((>= j 0)) ((set! j (+ j -1)))
      (set! m (remainder i 10))
      (set! ,(at '(+ s 4 (* 4 j))) (+ m ,(char->integer #\0)))
      (set! i (/ (+ i (- m)) 10)))
    (return s))
  
  (function void init ()
    (set! extent (/ heap-size 2))
    (set! from-space extent)
    (set! top extent))
  
  (function void run ()
    
    (while 1
      (switch code-reg
        ,@blocks)))
    
  (return (("run" . run) ("new" . new) ("call" . call)
    ("newFrame" . new-frame))))

