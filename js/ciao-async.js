/*
 *  ciao-async.js
 *
 *  Async client for a Web Worker running the Ciao engine (see ciao-worker.js)
 *
 *  Copyright (C) 2017-2022 Jose F. Morales
 */

/* --------------------------------------------------------------------------- */

/* TODO: similar to (:- block) implementation */
/* TODO: 'reject' not supported */
/* TODO: Make sure that this passes the Promises tests suite: https://github.com/promises-aplus/promises-tests */

class CiaoPromiseProxy {
  constructor() {
    this.hasValue = false; /* has a value */
    this.value = null; /* value */
    this.handler = null; /* 'then' handler */
    this.next = null; /* temporary promise, when the handler is not yet there */
  }
  setValue(value) {
    if (typeof this.handler === 'function') { /* we have both handler and value, call */
      var ret = this.handler(value);
      if (typeof ret === 'undefined') {
        this.next.setValue(undefined); /* no value */
      } else {
        if (typeof ret.then === 'function') { /* assume some Thenable (a promise) */
          var this_next = this.next;
          ret.then(function(r) {
            this_next.setValue(r); /* propagate value */
          });
        } else {
          this.next.setValue(ret); /* just value (equivalent to a promise coercion) */
        }
      }
    } else { /* otherwise save the value */
      this.value = value;
      this.hasValue = true;
    }
  }
  /* Note: assume that handler returns undefined or a promise */
  then(handler) {
    if (this.hasValue === true) { /* have both! call handler */
      var ret = handler(this.value);
      if (typeof ret.then === 'function') { /* assume some Thenable (a promise) */
        return ret;
      } else { /* promise coercion */
        return CiaoPromiseProxy.resolve(ret);
      }
    } else { /* otherwise save the handler */
      var next = new CiaoPromiseProxy(); /* create temporary promise */
      this.next = next;
      this.handler = handler;
      return next;
    }
  }

  /* A promise with resolved value `v` */
  static resolve(v) {
    var p = new CiaoPromiseProxy();
    p.setValue(v);
    return p;
  }
}

/* --------------------------------------------------------------------------- */

/**
 * A `CiaoWorker` is a instance of a Ciao engine running on a Web Worker
 */
class CiaoWorker {
  constructor(ciao_root_URL) {
    var listeners = [];
    this.pending_queue = [];
    this.pending_loadEng = true;
    this.pending_boot = true;
    this.ciao_root_URL = ciao_root_URL;
    this.listeners = listeners;
    this.w = new Worker(ciao_root_URL + "build/bin/ciao-worker.js");
    this.w.onmessage = function(event) {
      if (event.data.isLog) { // TODO: debug only?
        console.log(event.data.msg);
      } else {
        listeners[event.data.id].setValue(event.data.ret);
        delete listeners[event.data.id]; /* undefine this array element */
      }
    };
  }

  /**
   * Perform a request to `this.w` (the worker running the Ciao
   * engine). It creates and returns a CiaoPromiseProxy, whose value
   * is set on completion.
   * @return {CiaoPromiseProxy} Ciao promise with the result of the call.
   */
  #async_() {
    /* TODO: cleaner way? */
    if (this.pending_queue.length > 0) { /* call queued calls before */
      var f = this.pending_queue.shift();
      var self = this;
      var my_args = arguments;
      return f(self).then(function(r) {
        return self.#async_.apply(self, my_args);
      });
    }

    var proxy = new CiaoPromiseProxy();
    /* set in listeners (get next free id or push) */
    var id = 0;
    for (var id = 0; id < this.listeners.length; id++) {
      if (this.listeners[id] === undefined) break;
    }
    if (id === this.listeners.length) {
      this.listeners.push(proxy);
    } else {
      this.listeners[id] = proxy;
    }
    this.w.postMessage({
      id: id, /* id of message in listener array */
      cmd: arguments[0],
      args: Array.prototype.slice.call(arguments, 1)
    });
    return proxy;
  }

  /**
   * Ensure the engine is loaded correctly.
   */
  ensure_loadEng() {
    if (this.pending_loadEng) {
      this.pending_loadEng = false;
      this.pending_queue.push(function(self) {
        return self.#async_('loadEng', self.ciao_root_URL);
      });
    }
  }

  /**
   * Ensure boot.
   */
  ensure_boot() {
    this.ensure_loadEng();
    if (this.pending_boot) {
      this.pending_boot = false;
      this.pending_queue.push(function(self) {
        return self.#async_('boot');
      });
    }
  }

  /**
   * Use the bundle passed as a parameter, loading the engine if needed.
   * @param {string} name - Name of the bundle. 
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  use_bundle(name) {
    this.ensure_loadEng();
    return this.#async_('useBundle', name);
  };

  wait_no_deps() {
    this.ensure_loadEng();
    return this.#async_('waitNoDeps');
  }

  /**
   * Get the Ciao root path.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  get_ciao_root() {
    this.ensure_loadEng();
    return this.#async_('get_ciao_root');
  };

  /**
   * Get the stats of the latest call.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  get_stats() {
    this.ensure_loadEng();
    return this.#async_('get_stats');
  };

  /**
   * Show boot information in a JS console (this forces boot).
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  bootInfo() {
    this.ensure_boot();
    return this.#async_('bootInfo');
  };

  /**
   * Begins the query passed as parameter and obtains one solution. The
   * decision tree stays awake and waits for user's input.
   * @param {string} goal - Query to be launched.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  query_one_begin(goal) {
    this.ensure_boot();
    return this.#async_('query_one_begin', goal);
  }

  /**
   * Tells whether the query has started correctly and has more solutions
   * or not.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  query_ok() {
    return this.#async_('query_ok');
  }

  /**
   * Obtain the next solution for the query previously launched.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  query_one_next() {
    return this.#async_('query_one_next');
  }

  /**
   * End the query process stopping its decision tree.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  query_end() {
    return this.#async_('query_end');
  }

  /**
   * Read file specified in path parameter.
   * @param {string} path - Path of the file to read.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  readFile(path) {
    this.ensure_boot();
    return this.#async_('readFile', path);
  };

  /**
   * Write a string in a file.
   * @param {string} path - Path of the file to write.
   * @param {string} contents - String of the contents to write in the file.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  writeFile(path, contents) {
    this.ensure_boot();
    return this.#async_('writeFile', path, contents);
  };

  /**
   * Capture the output of the most recent query/ies.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  readStdout() {
    return this.#async_('readStdout');
  };

  /**
   * Capture the standard error of the most recent query/ies.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  readStderr() {
    return this.#async_('readStderr');
  };

  /**
   * Terminate the worker `w`. It does not finish its operations; it is 
   * stopped at once.
   * @returns undefined
   */
  terminate() {
    return this.w.terminate();
  }
}
