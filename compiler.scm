(import
  (scheme base)
  (scheme write)
  (rapid error)
  (rapid load)
  (rapid expand)
  (rapid import-set)
  (rapid compile)
  (rapid library))




; TODO: The compilation is completely missing  
(handle-compile-error
  (display
    (expand (load)))
  (newline))

