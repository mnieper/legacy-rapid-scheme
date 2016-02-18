(define-record-type table-type
  (%make-table entries)
  table?
  (entries table-entries table-set-entries!))

(define (make-table) (%make-table '()))

(define table-ref
  (case-lambda
   ((table key)
    (table-ref table key (lambda () (error "table-ref: not contained in hash table"
					   key))))
   ((table key thunk)
    (cond
     ((assv key (table-entries table)) => cdr)
     (else (thunk))))))

(define (table-set! table key value)
  (define entries (table-entries table))
  (cond
   ((assv key entries)
    => (lambda (entry)
	 (set-cdr! entry value)))
   (else
    (table-set-entries! table (cons (cons key value) entries)))))
    
(define (table-intern! table key failure)
  (define entries (table-entries table))
  (cond
   ((assv key entries) => cdr)
   (else
    (let ((value (failure)))
      (table-set-entries! table (cons (cons key value) entries))
      value))))
