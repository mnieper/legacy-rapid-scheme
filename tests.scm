#!/usr/bin/env scheme-r7rs

(import
  (scheme base)
  (rapid test)
  (rapid list)
  (rapid import-set))
  
(test-begin "Rapid-Scheme")

(test-begin "List")

(test '(1 3 5 7) (remove even? '(1 2 3 4 5 6 7 8)))

(test-end)

(test-begin "Import sets")

(let ()
  (define export-sets
    '(
      ((library 1)
        ((identifier1 value1) (identifier2 value2)))
      ((library 2)
        ((identifier3 value3) (identifier4 value4)))))
        
  (test '((identifier3 value3) (identifier4 value4))
    (import-set '(library 2) export-sets))
      
  (test '((identifier1 value1))
    (import-set '(only (library 1) identifier1) export-sets))

  (test '((identifier1 value1))
    (import-set '(except (library 1) identifier2) export-sets))
    
  (test '((prefix-identifier3 value3) (prefix-identifier4 value4))
    (import-set '(prefix (library 2) prefix-) export-sets))
    
  (test '((identifier5 value1) (identifier2 value2))
    (import-set '(rename (library 1) (identifier1 identifier5)) export-sets)))

(test-end)

(test-end)


  
