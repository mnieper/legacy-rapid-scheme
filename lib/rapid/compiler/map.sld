(define-library (rapid compiler map)
  (export make-map map? map-lookup map-insert)
  (import (scheme base))
  (include "map.scm"))
