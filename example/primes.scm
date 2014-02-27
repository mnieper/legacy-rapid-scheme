;;
;; Example program in pre-scheme
;;

; This program calculates and displays the number of primes between 1 and a fixed number in a rather naive way.

((case-lambda
    ((n isprim count)
      (set! n 10000)

      (write-string "Number of primes between 1 and ")
      (write-string (number->string n))
      (write-string ": ")

      (set! isprim
        (case-lambda
          ((a) (isprim a 2))
          ((b i)
            (if (< b (* 2 i))
              #t
              (if (= (truncate-remainder b i) 0)
                #f
                (isprim b (+ i 1)))))))

      (set! count
        (case-lambda
          ((c)
            (if (< c 2)
              0
              (if (isprim c)
                (+ (count (- c 1)) 1)
                (count (- c 1)))))))

      (write-string (number->string (count n)))
      (write-string "\n")))
  #f #f #f)

