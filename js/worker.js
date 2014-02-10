'use strict';

var module;

var heap = new ArrayBuffer(Math.pow(2, 20));

var foreign = {

  getHeapSize: function () {
    return Math.pow(2, 20);
  },

  writeString: function (p) {
    var h = new Uint8Array(heap, p);
    var s = decodeString(h);
    postMessage({cmd: 'output', msg: s});
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

function run() {
  try {
    module();
  }
  catch (c) {
    if (c instanceof Error) {
      postMessage({cmd: 'error', msg: c.message});
      postMessage({cmd: 'exit', msg: false});
    }
  }
}

function decodeString(h) {
  var s = '', p = 0, c, i;
  while (i = h[p++]) {
    if (i & 0x80) {
      if (i & 0x20) {
        c = (i & 0x0f) + (h[p++] & 0x3f) << 4 + (h[p++] & 0x3f) << 10;
      } else {
        c = (i & 0x1f) + (h[p++] & 0x3f) << 5;
      }
    } else {
      c = i;
    }
    s += String.fromCharCode(c);
  }
  return s;
};

onmessage = function (event) {
  var data = event.data;
  switch (data.cmd) {
  case 'execute':
    importScripts(data.msg);
    module = RapidModule(self, foreign, heap);
    run();
    break;
  }
};

