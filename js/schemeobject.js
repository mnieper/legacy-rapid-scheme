'use strict';

rapid.SchemeObject = function SchemeObject() {
};

rapid.SchemeObject.prototype.toBoolean = function toBoolean() {
  return true;
};

rapid.SchemeBoolean = function SchemeBoolean(boolean) {
  this._boolean = boolean;
};
rapid.inherits(rapid.SchemeBoolean, rapid.SchemeObject);

rapid.SchemeBoolean.prototype.toBoolean = function toBoolean() {
  return this._boolean;
};

rapid.SchemeBoolean.true = new rapid.SchemeBoolean(true);
rapid.SchemeBoolean.false = new rapid.SchemeBoolean(false);

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

