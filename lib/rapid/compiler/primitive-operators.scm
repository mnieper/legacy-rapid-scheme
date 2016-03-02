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

(define operator-cons (make-operator 'cons))
(define operator-car (make-operator 'car))
(define operator-cdr (make-operator 'cdr))
(define operator-pair? (make-operator 'pair?))
(define operator-null? (make-operator 'null?))
(define operator-list->vector (make-operator 'list->vector))
		      
(define operator+
  (make-operator '+))

(define operator-apply
  (make-operator 'apply))

(define operator-display
  (make-operator 'display))

(define operator-newline
  (make-operator 'newline))

(define operator-string-append
  (make-operator 'string-append))
