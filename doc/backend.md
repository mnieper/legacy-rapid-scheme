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
0xxx xxxx xxxx x010 | Procedure with a pointer to a closure on the heap
xxxx xxxx xxxx xxxx | Label
xxxx xxxx xxxx xxxx | Integer

Heap elements | Format                    | Interpretation
--------------------------------------------------------------------------------------------
Closure       | label pointer             | label: points to procedure's body; pointer: to parent frame
Frame         | integer pointer value ... | integer: number of values; pointer: to parent frame; value... : arguments
String data   | 0x0000 integer bytes ...  | 0x0000: type of string; integer: length of string; bytes: utf-8 encoded

