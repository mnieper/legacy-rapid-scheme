'use strict';

var module, cont;

var heap = new ArrayBuffer(Math.pow(2, 27));

var i32 = new Int32Array(heap);

var
  NULL_POINTER = 0x00000004,
  POINTER_MASK = 0x8ffffffc,
  TAG_MASK = 0xe0000000,
  SIZE_MASK = ~TAG_MASK,
  STRING_TAG = 0x40000000;

var foreign = {

  heapSize: Math.pow(2, 27),

  writeString: function (k, p) {
    var h = new Int32Array(heap, p & POINTER_MASK);
    if ((h[0] & TAG_MASK) != STRING_TAG) {
      foreign.typeError();
    }
    var s = decodeString(h);
    postMessage({cmd: 'output', msg: s});
    throw function () {
      module.call(k, NULL_POINTER);
    };
  },

  typeError: function () {
    postMessage({cmd: 'error', msg: 'argument type mismatch\n'});
    foreign.exit(1);
  },

  memoryError: function () {
    postMessage({cmd: 'error', msg: 'out of memory\n'});
    foreign.exit(1);
  },

  applicationError: function () {
    postMessage({cmd: 'error', msg: 'non-procedure application\n'});
    foreign.exit(1);
  },

  callError: function () {
    postMessage({cmd: 'error', msg: 'procedure called with wrong number of arguments\n'});
    foreign.exit(1);
  },

  exit: function (code) {
    postMessage({cmd: 'exit', msg: code !== 0x00000001});
    throw undefined;
  }

};

function run(msg) {
  try {
    cont(msg);
  }
  catch (c) {
    if (c instanceof Error) {
      postMessage({cmd: 'error', msg: c.message});
      postMessage({cmd: 'exit', msg: false});
    } else {
      cont = c;
    }
  }
}

function decodeString(h) {
  var s = '', p = 0, q = (h[p] & SIZE_MASK) + (p++), c, i;
  while (p < q) {
    s += String.fromCharCode(h[p++]);
  }
  return s;
};

onmessage = function (event) {
  var data = event.data;
  switch (data.cmd) {
  case 'execute':
    importScripts(data.msg);
    module = RapidModule(self, foreign, heap);
    cont = module.run;
    run();
    break;
  case 'continue':
    run(data.msg);
    break;
  }
};

