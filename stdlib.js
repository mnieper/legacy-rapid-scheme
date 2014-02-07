'use strict';

// TODO: Use rapid namespace

function inherits(childCtor, parentCtor) {
  return Object.defineProperties(childCtor, {
    prototype: {
      value: Object.create(parentCtor, {
        constructor: {
          value: childCtor
        }
      })
    }
  });
}

function SchemeObject() {
  
};

// TODO: schreibe exit als Prozedur wie hier um (oder im Linker)

function Procedure(code) {
  SchemeObject.call(this);
  this.code = code;
}
inherits(Procedure, SchemeObject);

function trampoline(thunk) {
  var procedure;
  while (1) {
    procedure = thunk.pop();
    thunk = procedure.code(thunk);
  }
}

function display(obj) {
  'use strict';
  postMessage(obj.toString());
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
var exit = new Procedure(function exit() {
  'use strict';
  postMessage('EXIT'); // FIXME
  throw exit;
});

function init(continuation) {
  self.onmessage = function (event) {
    'use strict';
    try {
      trampoline([event.data, continuation]);
    } catch (c) {
      continuation = c;
    };
  };
};

