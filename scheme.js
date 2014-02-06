// rename to rapid namespace

this.Scheme = (function () {
  'use strict';

  function toString() {
    return '[object Scheme]';
  }
  
  function program(scriptURL) {
    var worker;
    
    // function compile analog?

    function run() {
      worker = new Worker(scriptURL);
      worker.onmessage = function (event) {
        console.log("Worker sent: ", event.data); /* XXX */
      };
      worker.onerror = function (event) {
        console.log(event);
      };
      worker.postMessage('GO!'); /* XXX */ 
      console.log("Worker started.");
    };
    
    return {
      run: run
    }
  }
  
  return Object.create(Object.prototype, {
      program: { value: program },
      toString: { value: toString }
  });
})();

