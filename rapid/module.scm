(write-module
  "\
  function RapidModule(stdlib,foreign,heap){\
    'use asm';\
    var a=0;\
    var i=0;\
    var f=8;\
    var e=0;\
    var p=0;\
    var s=0;\
    var exit=foreign.exit;\
    var writeString=foreign.writeString;\
    var memoryError=foreign.memoryError;\
    var callError=foreign.callError;\
    var applicationError=foreign.applicationError;\
    var h32=new stdlib.Int32Array(heap);\
    var hu8=new stdlib.Uint8Array(heap);\
    function alloc(s){\
      s=s|0;\
      var p=0;\
      p=f;\
      f=(f+s)|0;\
      if((f|0)>1000000){memoryError();}\
      return p|0;\
    }\
    function procedure(l,f){\
      l=l|0;\
      f=f|0;\
      var p=0;\
      p=alloc(8)|0;\
      h32[p>>2] = l;\
      h32[(p+4)>>2] = f;\
      p=p|0x02;\
      return p|0;\
    }\
    function sum(i1,i2){/*FIXME: sum gets a vector, list*/\
      i1=i1|0;\
      i2=i2|0;\
      return (i1+i2)|0;\    
    }\
    function frame(n){\
      n=n|0;\
      var p=0;\
      p=alloc(((n+3)<<2)&0xfffffff8)|0;\
      h32[p>>2]=n;\
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

