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

(define-library (rapid compiler syntactic-environment)
  (export make-syntactic-environment
	  syntactic-environment?
	  lookup-denotation
	  lookup-syntax
	  insert-binding
	  delete-binding
	  insert-binding-from
	  insert-bindings-from
	  capture-references
	  derive-syntactic-environment)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme cxr)
	  (rapid compiler map)
	  (rapid compiler read)
	  (rapid compiler error))
  (include "syntactic-environment.scm"))
