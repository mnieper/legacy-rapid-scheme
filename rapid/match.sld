(define-library (rapid match)
  (export match)
  (import (scheme base))
  (begin
  
    (define-syntax match
      (syntax-rules ()
        ((match expr (pat . body) ...)
          
        
          ...)))))
          
          
          
          ; idea : if there are 
          
      
    (match a b)   muß also Macro sein; was machen wir, wenn a erst im Programm-Code kommt?
    
      
      
    (match (1 2 3) (b)
      (a b c)    b)
      
    
    (match ('quote 2 3)
      ('quote b c) b)
      
      
  (dei
