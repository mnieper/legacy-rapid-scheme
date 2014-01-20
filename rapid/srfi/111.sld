(define-library (rapid srfi 111)
  (export box box? unbox set-box!)
  (import (scheme base))
  (begin
    (define-record-type box-type
      (box value)
      box?
      (value unbox set-box!))))

