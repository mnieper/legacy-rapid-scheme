(write-module
  "\
  function RapidModule(stdlib,foreign,heap){\
    'use asm';\
    var a=0;\
    var i=0;\
    var f=8;\
    var e=0;\
    var p=0;\
    var exit=foreign.exit;\
    var writeString=foreign.writeString;\
    var callError=foreign.callError;\
    var h32=new stdlib.Int32Array(heap);\
    var hu8=new stdlib.Uint8Array(heap);\
    function alloc(s){\
      s=s|0;\
      var p=0;\
      p=f;\
      f=(f+s)|0;\
      return p|0;\
    }\
    function procedure(l){\
      l=l|0;
      
    }\
    function frame(n){\
      n=n|0;\
      var p=0;\
      p=alloc(((n+3)<<4)&0xfffffff8);\
      h32[p>>2]=n;\
      return p;\
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

