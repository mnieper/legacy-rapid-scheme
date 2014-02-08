/* TODO
* Add a number of constants, e.g.
*/

'use strict';
var rapid = {};

rapid.error = function error(message /* irritants missing */) {
  postMessage({cmd: 'error', msg: message.toString()});
  rapid.exit(rapid.SchemeBoolean.false);
};

rapid.callError = function callError() {
  postMessage({cmd: 'error', msg: 'procedure called with wrong number of arguments'});
  rapid.exit(rapid.SchemeBoolean.false);
};

rapid.inherits = function inherits(childCtor, parentCtor) {
  return Object.defineProperties(childCtor, {
    prototype: { value: Object.create(parentCtor, {
      constructor: { value: childCtor }
    })}
  });
};

importScripts("schemeobject.js");

rapid.display = function display(obj) {
  // This is more string-write
  postMessage({cmd: 'output', msg: obj.toString()});
};

rapid.sum = function sum(obj1, obj2) {
  return obj1 + obj2; // TODO
};

rapid.difference = function difference(obj1, obj2) {
  return obj1 - obj2; // TODO, see above
};

rapid.equality = function equality(obj1, obj2) {
  return new rapid.SchemeBoolean(obj1 === obj2); // TODO
};

rapid.exit = function exit(code) {
  code = code !== undefined ? code : new rapid.SchemeBoolean(true);
  postMessage({cmd: 'exit', msg: code.toBoolean()});
  throw undefined;
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

