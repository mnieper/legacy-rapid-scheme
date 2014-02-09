/* TODO
* Add a number of constants, e.g.
*/

'use strict';

importScripts("/js/base.js", "/js/schemeobject.js", "/js/operator.js");

rapid.error = function error(message /* irritants missing */) {
  postMessage({cmd: 'error', msg: message.toString()});
  rapid.exit(rapid.SchemeBoolean.false);
};

rapid.callError = function callError() {
  postMessage({cmd: 'error', msg: 'procedure called with wrong number of arguments'});
  rapid.exit(rapid.SchemeBoolean.false);
};

