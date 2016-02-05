(define-record-type set-type
  (%make-set elements)
  set?
  (elements set-elements set-set-elements!))

(define (make-set) (%make-set '()))

;; This procedure does not allocate a new set, contrary to set-adjoin of SRFI 113.
(define (set-adjoin set obj)
  (define elements (set-elements set))
  (if (memq obj elements) set (%make-set (cons obj elements))))

(define (set-contains? set obj)
  (and (memq obj (set-elements set)) #t))

(define (set-fold proc nil set)
  (let loop ((elements (set-elements set)))
    (if (null? elements)
	nil
	(proc (car elements) (loop (cdr elements))))))
