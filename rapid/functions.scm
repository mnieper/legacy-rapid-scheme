; TODO Locals instead of js-id!
; TODO Renames of js functions e.g = , < , ... (als macro...) ??? what about call?

(define-functions functions

  ; TODO Auto signed...
  (signed alloc ((signed s))
    (signed p)
    (js-assignment p free-ptr)
    (js-assignment free-ptr (js-signed (js-+ free-ptr s)))
    (js-if (js-> (js-signed free-ptr) heap-size) (js-call memory-error))
    (return p)))
    
  ;(void gc ()))



;(= p free-ptr)
;(= free-ptr (int+ free-ptr s))
;(if (int> free-ptr heap-size) (call memory-error))

