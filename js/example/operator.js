'use strict';

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

rapid.product = function product(obj1, obj2) {
  return obj1 * obj2; // FIXME
};

rapid.truncateRemainder = function truncateRemainder(obj1, obj2) {
  return obj1 % obj2; // TODO
};

rapid.isIncreasing = function isIncreasing(obj1, obj2) {
  return new rapid.SchemeBoolean(obj1 < obj2);
};

rapid.equality = function equality(obj1, obj2) {
  return new rapid.SchemeBoolean(obj1 === obj2); // TODO
};

rapid.exit = function exit(code) {
  code = code !== undefined ? code : new rapid.SchemeBoolean(true);
  postMessage({cmd: 'exit', msg: code.toBoolean()});
  throw undefined;
};

