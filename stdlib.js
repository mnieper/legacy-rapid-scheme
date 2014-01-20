function trampoline(thunk) {
  'use strict';
  while (1) {
    thunk = thunk();
  };
};

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

function exit() {
  'use strict';
  postMessage('EXIT'); // FIXME
  throw exit;
}

function init(continuation) {
  self.onmessage = function (event) {
    'use strict';
    try {
      trampoline(continuation(event.data));
    } catch (c) {
      continuation = c;
    };
  };
};

