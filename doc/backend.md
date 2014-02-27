Overview
========

Internal Types
--------------

Type                | Word 1 |  Word 2    |  Word 3    |
------------------------------------------------------
Exact integer value | 


The following datatypes are recognized in the compiled code:

Format for scalars  | Interpretation
--------------------------------------------------------------------------------
xxxx xxxx xxxx xxx0 | Exact integer between -2^30 and 2^30-1 in two's complement
0000 0000 0000 0001 | Boolean false
0000 0001 0000 0001 | Boolean true
0xxx xxxx xxxx x011 | Procedure with a pointer to a closure on the heap
xxxx xxxx xxxx xxxx | Label
xxxx xxxx xxxx xxxx | Integer

Heap elements | Format                    | Interpretation
--------------------------------------------------------------------------------------------
Closure       | label pointer             | label: points to procedure's body; pointer: to parent frame
Frame         | integer pointer value ... | integer: number of values; pointer: to parent frame; value... : arguments
String data   | 0x0000 integer bytes ...  | 0x0000: type of string; integer: length of string; bytes: utf-8 encoded




Scalar value
------------

x : i32.

mask = x8000.0007

if (x & 1) == 0 => integer
else if (x & mask) == 1 => boolean
else if (x & mask) == 8000.0000 => ptr
else if (x & mask) == 8000.0007 => null ptr



Pointer-Format
--------------

; Nan = 13 top-bits gesetzt; wir nehmen sogar 14 top-bits.
; Es bleiben: 32+18 bits.
; Das ist problematisch für, z.B. cons (Länge 2x4, denn 18 bits können keinen
; Pointer aufnehmen)
; Alternative: graue Objekte sind skalare... Vom oberen Teil des Heaps???
; Also doch stack???

Byte 0 = 
