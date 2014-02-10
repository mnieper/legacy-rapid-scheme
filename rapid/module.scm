(write-module
  "\
  function RapidModule(stdlib,foreign,heap){\
    'use asm';\
    var i=0;\
    var r0=0,r1=0,r2=0,r3=0,r4=0,r5=0,r6=0,r7=0;\
    var f=0;\
    var exit=foreign.exit;\
    var writeString=foreign.writeString;\
    var h32=new stdlib.Int32Array(heap);\
    var hu8=new stdlib.Uint8Array(heap);\
    function alloc(s){\
      s=s|0;\
      var p=0;\
      p=f;\
      f=(f+s)|0;\
      return p|0;\
    }\    
    function run(){\
      while(1){\
        switch(i|0){"
          code"\
        }\
      }\
    }\
    return run;\
  }")

