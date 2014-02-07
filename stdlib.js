'use strict';
var rapid = {};

rapid.error = function error(message /* irritants missing */) {
  postMessage(message.toString());
  throw exit;
};

rapid.callError = function callError() {
  postMessage('procedure called with wrong number of arguments');
    // Add type field to message
  throw exit; // TODO Add exit code
};

rapid.inherits = function inherits(childCtor, parentCtor) {
  return Object.defineProperties(childCtor, {
    prototype: {
      value: Object.create(parentCtor, {
        constructor: {
          value: childCtor
        }
      })
    }
  });
};

rapid.SchemeObject = function SchemeObject() {
};

rapid.SchemeString = function (string) {
  rapid.SchemeObject.call(this);
  this._string = string;
};
rapid.inherits(rapid.SchemeString, rapid.SchemeObject);

rapid.SchemeString.prototype.toString = function toString() {
  return this._string;
};

rapid.Procedure = function Procedure(code) {
  rapid.SchemeObject.call(this);
  this.code = code;
};
rapid.inherits(rapid.Procedure, rapid.SchemeObject);

rapid.trampoline = function trampoline(thunk) {
  var procedure;
  while (1) {
    procedure = thunk.pop();
    thunk = procedure.code(thunk);
  }
};

function display(obj) {
  // This is more string-write
  postMessage({cmd: 'output', msg: obj.toString()});
}

function sum(obj1, obj2) {
  'use strict';
  return obj1 + obj2; // TODO
}

function difference(obj1, obj2) {
  'use strict';
  return obj1 - obj2; // TODO, see above
}

function equality(obj1, obj2) {
  return obj1 === obj2; // TODO
}

// TODO: When used as a value, make it into a procedure not an operator.
function exit(code) {
  // TODO: turn code into a Javascript object
  postMessage({cmd: 'exit', msg: code});
  throw exit;
}

function init(continuation) {
  self.onmessage = function (event) {
    'use strict';
    try {
      rapid.trampoline([event.data, continuation]);
    } catch (c) {
      continuation = c;
    };
  };
}

