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

;;; SRFI-1 list-processing library 			-*- Scheme -*-
;;; Reference implementation
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

(define (take list k)
  (let loop ((list list) (k k))
    (if (zero? k)
	'()
	(cons (car list) (loop (cdr list) (- k 1))))))

(define (drop list k)
  (let loop ((list list) (k k))
    (if (zero? k)
	list
	(loop (cdr list) (- k 1)))))

(define (take-right list k)
  (let loop ((list list) (ref (drop list k)))
    (if (pair? ref)
	(loop (cdr list) (cdr ref))
	list)))

(define (drop-right list k)
  (let loop ((list list) (ref (drop list k)))
    (if (pair? ref)
	(cons (car list) (loop (cdr list) (cdr ref)))
	'())))

(define (circular-list? obj)
  (let loop ((fast obj) (slow obj))
    (and (pair? fast)
	 (let ((fast (cdr fast)))
	   (and (pair? fast)
		(let ((fast (cdr fast))
		      (slow (cdr slow)))
		  (or (eq? fast slow)
		      (loop fast slow))))))))

(define (any pred clist)
  (let loop ((clist clist))
    (if (null? clist)
	#f
	(or (pred (car clist)) (loop (cdr clist))))))

(define (every pred clist)
  (let loop ((clist clist))
    (if (null? clist)
	#t
	(and (pred (car clist)) (loop (cdr clist))))))

(define (map-in-order proc list)  
  (let loop ((list list))
    (if (null? list)
	'()
	(let ((value (proc (car list))))
	  (cons value (loop (cdr list)))))))
