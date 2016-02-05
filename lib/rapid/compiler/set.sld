(define-library (rapid compiler set)
  (export make-set set? set-contains set-fold)
  (import (scheme base))
  (include "set.scm"))
