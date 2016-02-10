(import (scheme base)
	(scheme process-context)
	(rapid format))

(define help-string (format "\
Usage: ~a [options] file
Options:
  -h, --help     Display this information
  -v, --version  Display compiler version information

" (car (command-line))))

(define version-string "\
rapid-compiler (Rapid Scheme) 0.1
Copyright (C) 2016 Marc Nieper-Wißkirchen
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

")

(define (help) (write-string help-string))
(define (version) (write-string version-string))

(cond
 ((or (member "--help" (command-line) string=?)
      (member "-h" (command-line) string=?))
  (help))
 ((or (member "--version" (command-line) string=?)
      (member "-v" (command-line) string=?))
  (version))
 (else
  (unless (= (length (command-line)) 2)
    (write-string "rapid-compiler: exactly one input file needed\n")
    (exit #f))
  'compile))
