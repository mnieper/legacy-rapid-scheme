'use strict';
var rapid = {};

rapid.Program = function Program(scriptURL) {
  this._scriptURL = scriptURL;
};

rapid.Program.prototype.run = function run() {
  var worker = new Worker(this._scriptURL);
  worker.onmessage = this._onmessage.bind(this);
  worker.onerror = this._onerror.bind(this);
  worker.postMessage('GO!'); // XXX
};

rapid.Program.prototype._onmessage = function _onmessage(event) {
  console.log("Worker sent: ", event.data); // XXX
};

rapid.Program.prototype._onerror = function _onerror(event) {
  console.log(event); // TODO
};

