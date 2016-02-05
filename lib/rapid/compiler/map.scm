;;; Maps
(define-record-type map-type
  (%make-map default-map compare entries)
  map?
  (default-map map-default-map)
  (compare map-compare)
  (entries map-entries map-set-entries!))

(define (make-map default-map compare)
  (%make-map default-map compare '()))

(define (map-lookup map key)
  (cond
   ((assoc key (map-entries map) (map-compare map)) => cdr)
   (else ((map-default-map) key))))

(define (map-insert map key value)
  (%make-map (map-default-map map) (cons (cons key value) (map-entries map)) (map-compare map)))
