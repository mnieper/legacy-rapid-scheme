(set! f
  (case-lambda
    ((n)
      (if (= 0 n)
        0
        (+ n (f (- n 1)))))))
      
(display (f 2000))

(display
  (+ 1 (call/cc
         (case-lambda
           ((k) (+ 2 (k 3)))))))

