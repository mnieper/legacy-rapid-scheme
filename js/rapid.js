'use strict';
var rapid = {};

rapid.empty = function empty() {
};

rapid.inherits = function inherits(childCtor, parentCtor) {
  return Object.defineProperties(childCtor, {
    prototype: { value: Object.create(parentCtor.prototype, {
      constructor: { value: childCtor }
    })}
  });
};

/*
 * TODO Emulate a VT100. 
 */
rapid.Console = function Console(parent) {
  this._parent = parent;
  this._document = parent.ownerDocument;
  this._output = parent.appendChild(this._document.createElement('pre'));
  this._output.classList.add('rapid-output');
};

rapid.Console.prototype.write = function write(s) {
  // XXX Will it scroll? See issue
  this._output.appendChild(this._document.createTextNode(s));
};

rapid.Console.prototype.error = function error(s) {
  var span = this._output.appendChild(this._document.createElement('span'));
  span.classList.add('rapid-error');
  span.appendChild(this._document.createTextNode(s));
};

rapid.Program = function Program(scriptURL) {
  this._scriptURL = scriptURL;
  this._worker = null;
  this.onoutput = rapid.empty;
  this.onerror = rapid.empty;
  this.onexit = rapid.empty;
};

rapid.Program.prototype.run = function run() {
  this._worker = new Worker('js/worker.js');
  this._worker.onmessage = this._onmessage.bind(this);
  this._worker.onerror = this._onerror.bind(this);
  this._worker.postMessage({cmd: 'execute', msg: this._scriptURL}); // XXX Need command line.
};

rapid.Program.prototype.continue_ = function () {
  this._worker.postMessage({cmd: 'continue', msg: ''});
};

rapid.Program.prototype._onmessage = function _onmessage(event) {
  var data = event.data;
  switch (data.cmd) {
  case 'output':
    this.onoutput(data.msg);
    this.continue_();
    break;
  case 'error':
    this.onerror(data.msg);
    this.continue_();
    break;
  case 'exit':
    this._worker.terminate();
    this.onexit({code: data.msg});
    break;
  default:
    this._worker.terminate();
    this.onexit({code: 'error', command: data.cmd});
  }
};

rapid.Program.prototype._onerror = function _onerror(event) {
  this.onexit({
    code: 'error',
    message: event.message,
    filename: event.filename,
    lineno: event.lineno
  }); 
};

rapid.ConsoleProgram = function ConsoleProgram(scriptURL, console) {
  rapid.Program.call(this, scriptURL);
  this._console = console;
  this.onoutput = this._onoutput.bind(this);
  this.onerror = this._onerror.bind(this);
};
rapid.inherits(rapid.ConsoleProgram, rapid.Program);

rapid.ConsoleProgram.prototype._onoutput = function _onoutput(output) {
  this._console.write(output);
};

rapid.ConsoleProgram.prototype._onerror = function _onerror(error) {
  this._console.error(error);
};
