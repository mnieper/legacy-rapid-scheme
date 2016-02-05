(import (scheme base)
	(rapid compiler read)
	(rapid test))

(define (source-port-string string)
  (make-source-port (open-input-string string) #f ""))

(define (read-datum string)
  (define port (source-port-string string))
  (syntax->datum (read-syntax port)))

(define (read-data string)
  (define port (source-port-string string))
  (let loop ((datum* '()))
    (let ((syntax (read-syntax port)))
      (if (eof-object? syntax)
	  (reverse datum*)
	  (loop (cons (syntax->datum syntax) datum*))))))

(test-begin "compiler read")

(test-begin "source ports")

(test-assert "Allocating a source port yields a source port"
	     (source-port? (make-source-port (open-input-string "") #f "")))

(test-end "source ports")

(test-begin "read-syntax")

(test-equal "Booleans"
	    '(#t #f #t #f #t #f #t #f #t #f)
	    (read-data "#t #f #true #false #T #F #TRUE #FALSE #tRuE #False"))

(test-equal "Strings"
	    "The quick red fox jumped over the lazy dog."
	    (read-datum "\"The quick red fox jumped over the lazy dog.\""))

(test-equal "Escapes in strings"
	    "\r\n\t\b\a\|\"\\"
	    (read-datum "\"\\r\\n\\t\\b\\a\\|\\\"\\\\\""))

(test-equal "Hex escapes in strings"
	    "\x7f;Larceny\x00;#x21;"
	    (read-datum "\"\\x7f;\\x4c;\\x61;\\x72;\\x63;\\x65;\\x6e;\\x79;\\x0;#x21;\""))

(test-equal "Characters"
	    '(#\a #\Z #\0 #\9 #\` #\' #\" #\~ #\! #\=)	
	    (read-data "#\\a #\\Z #\\0 #\\9 #\\` #\\' #\\\" #\\~ #\\! #\\="))

(test-equal "Escapes in characters"
	    (map integer->char '(32 9 10 13))
	    (read-data "#\\  #\\\t #\\\n #\\\r"))

(test-equal "Character names"
	    '(#\alarm #\backspace #\delete #\escape #\newline)
	    (read-data "#\\alarm #\\backspace #\\delete #\\escape #\\newline"))

(test-equal "More character names"
	    '(#\null #\return #\space #\tab)
	    (read-data "#\\null #\\return #\\space #\\tab"))

(test-equal "Hex escapes in characters"
	    '(#\x0 #\x0 #\x1 #\x20 #\x5f #\x7c #\x7f)
	    (read-data "#\\x0 #\\x00 #\\x1 #\\x20 #\\x5f #\\x7c #\\x7f"))

(test-equal "Identifier initials"
	    '(a ! $ % & * / : < = > ? ^ _ ~ @)
	    (read-data "a ! $ % & * / : < = > ? ^ _ ~ @"))

(test-equal "Identifier subsequents"
	    '(Z: !z $0 %/ &? *^ /~ :@ <9 =+ >- ?@ ^+- _-+ ~@- @@@@@)
	    (read-data "Z: !z $0 %/ &? *^ /~ :@ <9 =+ >- ?@ ^+- _-+ ~@- @@@@@"))

(test-equal "Identifiers terminated by vertical lines"	       
	    '|;alskjdf;aqwjepojq-1945apgf ;bna]as|
	    (read-datum "|;alskjdf;aqwjepojq-1945apgf ;bna]as|"))

(test-equal "Escapes in identifiers"
	    '|\a\b\t\n\r\|\"\\|
	    (read-datum "|\\a\\b\\t\\n\\r\\|\\\"\\\\|"))

(test-equal "Several identifiers with vertical lines"
	    '(|\\\|\" a| |\"\|\\ b|)
	    (read-data "|\\\\\\|\\\" a| |\\\"\\|\\\\ b|"))

(test-equal "Control characters in identifiers"
	    '|\x0;\x1;\x2;\t\r\x41;\n\t\x7e;\x7f;|
	    (read-datum "|\\x0;\\x1;\\x2;\\t\\r\\x41;\\n\\t\\x7e;\\x7f;|"))

(test-equal "Explicit signs"
	    '(+ -)
	    (read-data "+ -"))

(test-equal "Explicit signs with subsequents"
	    '(+: -@ +- -- +@ -@ +$$ -@3 +-4 --5 +@_ -@.)
	    (read-data "+: -@ +- -- +@ -@ +$$ -@3 +-4 --5 +@_ -@."))

(test-equal "Explicit signs with dots"
	    (map string->symbol
		 '("+.!" "-.+" "+.." "-.." "+.@" "-.@"))
	    (read-data "+.! -.+ +.. -.. +.@ -.@"))

(test-equal "More explicit signs with dots"
	    (map string->symbol
		 '("+.<.+" "-.++." "+..:?" "-..-" "+.@&." "-.@24"))
	    (read-data "+.<.+ -.++. +..:? -..- +.@&. -.@24"))

(test-equal "Identifiers starting with a dot"
	    '(._ .+ .- .@ .. ._. .+. .-. .@. ...)
	    (read-data "._ .+ .- .@ .. ._. .+. .-. .@. ..."))

(test-equal "Decimal numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 97 1001)
	    (read-data "0 +0 -0 00 +0000 -00000 001 2 -003 007 97 1001"))

(test-equal "Exact numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 97 1001)
	    (read-data "#e0 #e+0 #e-0 #e00 #e+0000 #e-00000 \
                              #e001 #e2 #e-003 #e007 #e97 #e1001"))

(test-equal "Decimal numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 97 1001)	  
	    (read-data "#d0 #d+0 #d-0 #d00 #d+0000 #d-00000 \
                              #d001 #d2 #d-003 #d007 #d97 #d1001"))

(test-equal "Exact decimal numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 97 1001)
	    (read-data "#e#d0 #e#d+0 #e#d-0 #e#d00 #e#d+00 #e#d-0000 \
			      #e#d01 #e#d2 #e#d-3 #e#d007 #e#d97 #e#d1001"))

(test-equal "Decimal exact numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 97 1001)	  
	    (read-data "#d#e0 #d#e+0 #d#e-0 #d#e00 #d#e+00 #d#e-0000 \
                              #d#e01 #d#e2 #d#e-3 #d#e007 #d#e97 #d#e1001"))

(test-equal "Hexadecimal numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 151 4097 43981 -65244)
	    (read-data "#x0 #x+0 #x-0 #x00 #x+0000 #x-00000 \
                              #x001 #x2 #x-003 #x007 #x97 #x1001 \
                              #xabcd #x-fedc"))

(test-equal "Exact hexadecimal numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 151 4097 43981 -65244)
	    (read-data "#e#x0 #e#x+0 #e#x-0 #e#x00 #e#x+00 #e#x-0000 \
			      #e#x01 #e#x2 #e#x-3 #e#x007 #e#x97 #e#x1001 \
			      #xabcd #x-fedc"))

(test-equal "Hexadecimal exact numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 151 4097 43981 -65244)
	    (read-data "#x#e0 #x#e+0 #x#e-0 #x#e00 #x#e+00 #x#e-0000 \
                              #x#e01 #x#e2 #x#e-3 #x#e007 #x#e97 #x#e1001 \
                              #xabcd #x-fedc"))

(test-equal "Octal numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 63 513)
	    (read-data "#o0 #o+0 #o-0 #o00 #o+0000 #o-00000 \
                              #o001 #o2 #o-003 #o007 #o77 #o1001"))

(test-equal "Exact octal numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 63 513)
	    (read-data "#e#o0 #e#o+0 #e#o-0 #e#o00 #e#o+00 #e#o-0000 \
                              #e#o01 #e#o2 #e#o-3 #e#o007 #e#o77 #e#o1001"))

(test-equal "Octal exact numbers"
	    '(0 0 0 0 0 0 1 2 -3 7 63 513)
	    (read-data "#o#e0 #o#e+0 #o#e-0 #o#e00 #o#e+00 #o#e-0000 \
                              #o#e01 #o#e2 #o#e-3 #o#e007 #o#e77 #o#e1001"))

(test-equal "Binary numbers"
	    '(0 0 0 0 0 0 1 9 -63 170)
	    (read-data "#b0 #b+0 #b-0 #b00 #b+000 #b-000 \
                              #b001 #b1001 #b-111111 #b+10101010"))

(test-equal "Exact binary numbers"
	    '(0 0 0 0 0 0 1 9 -63 170)
	    (read-data "#e#b0 #e#b+0 #e#b-0 #e#b00 #e#b+000 #e#b-000 \
                              #e#b001 #e#b1001 #e#b-111111 #e#b+10101010"))

(test-equal "Binary exact numbers"
	    '(0 0 0 0 0 0 1 9 -63 170)
	    (read-data "#b#e0 #b#e+0 #b#e-0 #b#e00 #b#e+000 #b#e-000 \
                              #b#e001 #b#e1001 #b#e-111111 #b#e+10101010"))

(test-equal "Other tokens"
	    '(() #() #u8() 'x `x ,x ,@x (a . b))
	    (read-data "( ) #( ) #u8( ) 'x `x ,x ,@x (a . b)"))

(test-equal "Line comments"
	    '(yes but this is not)
	    (read-data "yes; this is a comment \nbut;\r\n this is;too\rnot"))

(test-equal "Datum comments"
	    '(1 3 6 9)
	    (read-data "1 #;2 3 #;(4 5) 6 #;(7 (8)) 9"))

(test-equal "Nested comments"
	    '(a comment this is not)
	    (read-data "#|yes; this is |#a comment #|\nbut;\r\n|# this is #;too\rnot"))

(test-equal "Nested nested comments"
	    '(and this)
	    (read-data "and #|they #|nest|# like|# this"))

(test-equal "Bytevectors"
	    '(#u8() #u8(0 1 2 3 255))
	    (read-data "#u8() #u8(0 1 2 3 255)"))

(test-equal "Lists"
	    '(() (1 . 2) (3 . (4 . (5 . ()))))
	    (read-data "() (1 . 2) (3 . (4 . (5 . ())))"))

(test-equal "Vectors"
	    '(#() #(a) #(19 21 c))
	    (read-data "#() #(a) #(19 21 c)"))

(test-equal "Datum labels"
	    (let* ((x (list 1 2 3 4 5))
		   (v (vector x x x)))
	      (set-car! (cddr x) v)
	      (set-cdr! (cdr (cdr (cdr x))) x)
	      (vector-set! v 1 (list v))
	      v)
	    (read-datum "#125=#(#213=(1 2 #125# 4 . #213#) (#125#) #213#)"))

(test-end "read-syntax")

(test-end)
