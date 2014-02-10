function RapidModule(stdlib, foreign, heap) {
  'use asm';

  var pc = 0;
  var i0 = 0, i1 = 0, i2 = 0, i3 = 0, i4 = 0, i5 = 0, i6 = 0, i7 = 0;
  var f = 0;
  var exit = foreign.exit;
  var writeString = foreign.writeString;
  var h32 = new stdlib.Int32Array(heap);
  var hu8 = new stdlib.Uint8Array(heap);
  
  function alloc(s) {
    s = s|0;
    var p = 0;
    p = f;
    f = (f + s)|0;
    return p|0;
  }

  function run() {
    while (1) {
      switch (pc|0) {
      case 0:
        // Initialize heap
        i0 = alloc(4)|0;
        hu8[i0] = 0x41;
        hu8[(i0 + 1)|0] = 0x42;
        hu8[(i0 + 2)|0] = 0x43;
        hu8[(i0 + 3)|0] = 0x00;
        pc = 1;
        break;
      case 1:
        writeString(i0|0);
        exit(0);
      }
    }
  }

  return run;
}
