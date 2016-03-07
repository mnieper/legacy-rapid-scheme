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

(define-library (rapid compiler syntactic-environments)
  (export make-syntactic-environment
	  syntactic-environment?
	  with-syntactic-environment
	  with-scope
	  with-isolated-references
	  make-denotation
	  denotation?
	  denotation-syntax
	  lookup-binding!
	  lookup-denotation!
	  lookup-syntax!
	  syntactic-binding?
	  syntactic-binding-syntax
	  binding-denotation
	  insert-binding!
	  insert-binding-from!
	  insert-bindings-from!
	  delete-binding!
	  derive-syntactic-environment
	  get-syntactic-environment
	  syntactic-environment)
  (import (scheme base)
	  (scheme case-lambda)
	  (rapid format)
	  (rapid comparators)
	  (rapid box)
	  (rapid sets)
	  (rapid maps)
	  (rapid compiler syntax)
	  (rapid compiler read)
	  (rapid compiler error))
  (include "syntactic-environments.scm"))
