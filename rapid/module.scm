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
    var m=0;"
    globals"\
    var imul=stdlib.Math.imul;\
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
      if((f|0)>100000000){memoryError();}\
      return p|0;\
    }\
    function procedure(l,f){\
      l=l|0;\
      f=f|0;\
      var p=0;\
      p=alloc(8)|0;\
      h32[p>>2] = l;\
      h32[(p+4)>>2] = f;\
      p=p|0x3;\
      return p|0;\
    }\
    function equality(i1,i2){\
      i1=i1|0;\
      i2=i2|0;\
      return ((i1|0)==(i2|0)?0x10001:0x1)|0;\
    }\
    function sum(i1,i2){/*FIXME: sum gets a vector, list*/\
      i1=i1|0;\
      i2=i2|0;\
      return (i1+i2)|0;\    
    }\
    function difference(i1,i2){\
      i1=i1|0;\
      i2=i2|0;\
      return (i1-i2)|0;\
    }\
    function product(i1,i2){\
      i1=i1|0;\
      i2=i2|0;\
      return ((imul(i1>>1,i2>>1)|0)<<1)|0;\
    }\
    function isIncreasing(i1,i2){\
      i1=i1|0;\
      i2=i2|0;\
      return ((i1|0)<(i2|0)?0x00010001:0x00000001)|0;\
    }\
    function truncateRemainder(i1,i2){\
      i1=i1|0;\
      i2=i2|0;\
      return ((i1>>1)%(i2>>1))<<1|0;\
    }\
    function numberToString(i){\
      i=i|0;\
      var n=0;\
      var j=0;\
      var d=0;\
      var p=0;\
      var m=0;\
      i=i>>1;\
      if((i|0)<0){i=-i|0;d=1;n=n+1|0;}\
      if((i|0)==0){n=1}else{\
        for(j=i;(j|0)!=0;j=(j|0)/10|0){n=n+1|0;}\
      }\
      s=alloc(n+16&0xfffffff8)|0;\
      h32[s>>2]=0x0;\
      h32[s+4>>2]=n|0;\
      if((d|0)==1){\
        hu8[s+8|0]=0x2d;\
      }\
      hu8[(s+8|0)+n|0]=0x0;\
      for(j=n-1|0;(j|0)>=0;j=j-1|0){\
        m=(i|0)%10|0;\
        hu8[(s+8|0)+j|0]=m+0x30|0;\
        i=((i-m)|0)/10|0;\
      }\
      return s|0;\
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

