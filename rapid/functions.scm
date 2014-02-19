(functions

  (js-function 'alloc '(s) 
    (js-signed-param 's)
    (js-signed-var 'p)
    (js-assignment 'p 'free-ptr)
    (js-assignment 'free-ptr (js-signed (js-+ 'free-ptr (js-& (js-+ 7 's) #xfffffff8))))
    (js-if (js-> (js-signed 'free-ptr) (js-signed 'heap-size)) (js-call 'memory-error))
    (js-return 'p))

  (js-function 'procedure '(label parent-frame-ptr)
    (js-signed-param 'label)
    (js-signed-param 'parent-frame-ptr)
    (js-signed-var 'p)
    (js-assignment 'p (js-signed (js-call 'alloc (js-number 8))))
    (js-assignment (heap-location 'p) 'label)
    (js-assignment (heap-location (js-+ 'p 4)) 'parent-frame-ptr)
    (js-assignment 'p (js-or 'p (js-number #x3)))
    (js-return (js-signed 'p)))  

  (js-function 'frame '(n)
    (js-signed-param 'n)
    (js-signed-var 'p)
    (js-assignment 'p (js-signed (js-call 'alloc (js-+ 8 (js-<< ' (js-number 2))))))
    (js-assignment (heap-location 'p) 'n)
    (js-return (js-signed 'p)))


  (js-function '= '(i1 i2)
    ; FIXME Implement full numeric tower
    (js-signed-param 'i1)
    (js-signed-param 'i2)
    (js-return (js-signed (js-conditional (js-== (js-signed 'i1) (js-signed 'i2)) true false))))

  (js-function '+ '(i1 i2)
    ; FIXME: Should be able to receive any number of parameters
    (js-signed-param 'i1)
    (js-signed-param 'i2)  
    (js-return (js-signed (js-+ 'i1 'i2))))
    
  (js-function '- '(i1 i2)
    ; FIXME: Should be able to receive any number of parameters
    (js-signed-param 'i1)
    (js-signed-param 'i2)  
    (js-return (js-signed (js-- 'i1 'i2))))

  (js-function '+ '(i1 i2)
    ; FIXME: Should be able to receive any number of parameters
    (js-signed-param 'i1)
    (js-signed-param 'i2)  
    (js-return (js-signed
      (js-<<
        (js-call 'imul (js->> 'i1 1) (js->> 'i2 1)) 1))))

  (js-function '< '(i1 i2)
    (js-signed-param 'i1)
    (js-signed-param 'i2)
    (js-return (js-signed (js-conditional (js-< (js-signed 'i1)  (js-signed 'i2)) true false))))
    
  (js-function 'truncate-remainder '(i1 i2)
    (js-signed-param 'i1)
    (js-signed-param 'i2)
    (js-return (js-signed (js-integer->value (js-% (js-value->integer 'i1) (js-value->integer 'i2))))))

  (function 'number->string '((signed i))
    (var i 0)
    (var j 0)
    (var n 0)
    (var p 0)
    (var d 0)
    (var m 0)
    (var s 0)
    
    (set! 'i (unbox-integer 'i))
    (if (< 'i 0)
      (begin
        (set! 'i (- 'i))
        (set! 'd 1)
        (set! 'n 1))
      (if (= 'i 0)
        (set! 'n 1)
        (for ((set! 'j 'i)) ((not= 'j 1)) ((set! 'j (/ 'j 10)))
          (set! 'n (+ 'n 1)))))
    (set! 's (call 'alloc (+ 8 (* 4 'n))))
    (set! (heap-location 's) string-tag)
    (set! (heap-location (+ 's 4)) 'n)
    (if (= 'd 1)
      (set! (heap-location (+ 's 8)) (char->integer #\-)))
   
    (for ((set! 'j (- 'n 1))) ((>= 'j 0)) ((set! 'j (- 'j 1)))
      (set! 'm (truncate-remainder 'i 10))
      (set! (heap-location (+ 's 8 'j)) (+ 'm (char->integer #\0)))
      (set! 'i (/ (+ 'i 'm) 10)))

    (return signed 's))
    
    )
