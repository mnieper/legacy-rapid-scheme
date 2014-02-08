'use strict';

var rapid = {};

rapid.inherits = function inherits(childCtor, parentCtor) {
  return Object.defineProperties(childCtor, {
    prototype: { value: Object.create(parentCtor, {
      constructor: { value: childCtor }
    })}
  });
};

rapid.trampoline = function trampoline(thunk) {
  var procedure;
  while (1) {
    procedure = thunk.pop();
    thunk = procedure.code(thunk);
  }
};

function init(continuation) {
  self.onmessage = function (event) {
    'use strict';
    try {
      rapid.trampoline([event.data, continuation]);
    } catch (c) {
      if (c instanceof Error) {
        postMessage({cmd: 'error', msg: c.message});
        postMessage({cmd: 'exit', msg: false});
        continuation = undefined;
      } else {
        continuation = c;
      }
    };
  };
}
