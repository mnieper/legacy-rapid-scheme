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

(define-library (rapid compiler primitive)
  (export primitive-environment)
  (import (scheme base)                  (scheme write) ;;; XXX
	  (scheme write)
	  (rapid lists)
	  (rapid sets)
	  (rapid comparators)
	  (rapid compiler error)
	  (rapid compiler syntax)
	  (rapid compiler expand)
	  (rapid compiler environments)
	  (rapid compiler macro-transformers)
	  (rapid compiler syntactic-closures)
	  (rapid compiler syntactic-environments)
	  (rapid compiler expressions)
	  (rapid compiler primitive-operators))
  (include "primitive.scm"))
