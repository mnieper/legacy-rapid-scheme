;;; Rapid Scheme --- An implementation of R7RS

;; Copyright (C) 2016 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
   (else ((map-default-map map) key))))

(define (map-insert map key value)
  (%make-map (map-default-map map)
	     (map-compare map)
	     (cons (cons key value) (map-entries map))))

(define (map-delete map key)
  (define compare (map-compare map))
  (%make-map (map-default-map map)
	     compare
	     (let loop ((entries (map-entries map)))
	       (cond
		((null? entries)
		 '())
		((compare (caar entries) key)
		 (cdr entries))
		(else
		 (cons (car entries) (loop (cdr entries))))))))

(define (map-map-keys proc m)
  (%make-map (map-default-map m)
	     (map-compare m)
	     (map
	      (lambda (entry)
		(cons (proc (car entry)) (cdr entry)))
	      (map-entries m))))

(define (map->alist map)
  (define compare (map-compare map))
  (let loop ((alist '()) (entries (map-entries map)))
    (if (null? entries)
	alist
	(let ((entry (car entries)))
	  (if (assoc (car entry) alist compare)
	      (loop alist (cdr entries))
	      (loop (cons entry alist) (cdr entries)))))))
