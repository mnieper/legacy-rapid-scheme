(import
  (scheme base)
  (scheme read)
  (rapid load)
  (rapid compile))

(write-string     
  (compile
    (read-file)))

