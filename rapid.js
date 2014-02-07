'use strict';
var rapid = {};

rapid.Program = function Program(scriptURL) {
  this._scriptURL = scriptURL;
  this._worker = null;
  this.onoutput = this._onoutput.bind(this);
};

rapid.Program.prototype.run = function run() {
  this._worker = new Worker(this._scriptURL);
  this._worker.onmessage = this._onmessage.bind(this);
  this._worker.onerror = this._onerror.bind(this);
  this._worker.postMessage({cmd: 'execute'}); // XXX
};

rapid.Program.prototype._onmessage = function _onmessage(event) {
  var data = event.data;
  switch (data.cmd) {
  case 'output':
    this.onoutput(data.msg);
    break;
  case 'exit': 
    this._worker.terminate();
    break;
  default:
    console.log("Unknown command", event.data); // XXX
  }
};

rapid.Program.prototype._onerror = function _onerror(event) {
  console.log(event); // TODO
};

rapid.Program.prototype._onoutput = function _onoutput(output) {
};
