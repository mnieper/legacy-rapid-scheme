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
