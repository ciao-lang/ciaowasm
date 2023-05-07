/*
 *  ciao-prolog.js
 *
 *  JavaScript interface to Ciao Prolog (wasm)
 *
 *  Copyright (C) 2017-2023 Jose F. Morales
 */

/* TODO: make Worker optional */

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
 * Function that contains the Worker code (do not call it directly)
 */

// TODO: make Web Worker optional
function ciao_worker_fun() {
  var root_url;
  var f={};
  var stdout = "";
  var stderr = "";
  // Pending loads (status and msg id)
  var pending_load = false;
  var pending_load_hook = undefined;

  var Ciao = (function() {
    var EMCiao = {}; // Emscripten module for CIAOENGINE
    var FS; // Emscripten FS

    var prev_time = null;
    function now() { return (new Date()).getTime(); }
    function startTimer() { prev_time = now(); }
    function checkTimer() { return (now() - prev_time); } /* milliseconds */

    var Ciao = {};
    Ciao.stats = {};   // Statistics
    Ciao.bundle = {};  // Bundle map
    Ciao.depends = []; // Bundle dependencies
    Ciao.ciao_root_URL = null; // URL for CIAOROOT (null when not initialized yet)

    globalThis.__emciao = EMCiao; // (make it visible for bundle preloader)

    Ciao.bind_funcs = function() {
      Ciao.init = EMCiao.cwrap('ciaowasm_init', 'number', ['string']);
      Ciao.boot = EMCiao.cwrap('ciaowasm_boot', 'number', []);
      Ciao.run = EMCiao.cwrap('ciaowasm_run', 'number', ['string']);
      Ciao.query_begin = EMCiao.cwrap('ciaowasm_query_begin', 'number', ['string']);
      Ciao.query_ok = EMCiao.cwrap('ciaowasm_query_ok', 'number', []);
      Ciao.query_suspended = EMCiao.cwrap('ciaowasm_query_suspended', 'number', []);
      Ciao.query_resume = EMCiao.cwrap('ciaowasm_query_resume', 'number', []);
      Ciao.query_next = EMCiao.cwrap('ciaowasm_query_next', 'number', []);
      Ciao.query_end = EMCiao.cwrap('ciaowasm_query_end', 'number', []);
    };

    function mkdir_noerr(path) {
      try { FS.mkdir(path); } catch(e) { /* ignore errors */ };
    }

    Ciao.preload_file = function(dir,relpath) {
      if (Ciao.ciao_root_URL === null) throw new Error('null ciao_root_URL');
      var srcurl = Ciao.ciao_root_URL + relpath;
      var srcdir = dir + '/' + relpath;
      var spath = srcdir.split('/');

      /* Create directory path */
      var len = spath.length - 1;
      var dir = "";
      if (len > 0) {
        dir += spath[0];
        mkdir_noerr(dir);
        for (var i = 1; i < len; i++) {
          dir += "/"+spath[i];
          mkdir_noerr(dir);
        }
      }
      var base = spath[len];

      /* Create preloaded file */
      //FS.createPreloadedFile(dir, base, srcurl, true, false);
      FS.createPreloadedFile(dir, base, srcurl, true, true); // rw access
      // TODO: only works in web workers, does not work in lpdoc example
      // FS.createLazyFile(dir, base, srcurl, true, true); // rw access
    };

    Ciao.preload_bundle = function(b) {
      Ciao.bundle[b].preload();
    };

    Ciao.collect_wksps = function() {
      let wksps = [];
      let seen = {};
      for (const j of Ciao.depends) {
        var wksp = Ciao.bundle[j].wksp;
        if (!seen[wksp]) {
          wksps.unshift(wksp); /* right order? */
          seen[wksp] = true;
        }
      }
      return wksps;
    };

    // Update timestamps of build/cache files
    // TODO: needs around 8ms
    // TODO: allow frozen workspaces in c_itf, inhibits recompilation!
    // TODO: update needed for dynamic use_bundle?
    Ciao.update_timestamps = function(wksps) {
      var tsnow = Date.now();
      for (const wksp of wksps) {
        const dir = wksp + "/build/cache";
        for (const f of FS.readdir(dir)) {
          if (f.endsWith('.po') || f.endsWith('.itf')) {
            const path = dir + "/" + f;
            // const timestamp = FS.lookupPath(path).node.timestamp;
            FS.utime(path, tsnow, tsnow);
          }
        }
      }
    };

    /* --------------------------------------------------------------------------- */
    /* Initialization */

    Ciao.init_emciao = function(ciao_root_URL, onready, onprintout, onprinterr) {
      /* Start the engine with hooks for initialization */
      EMCiao.noExitRuntime = true;
      EMCiao['locateFile'] = function(path, prefix) {
        var f;
        // custom dirs
        if (ciao_root_URL === null) throw new Error('null ciao_root_URL');
        if (path.endsWith(".mem")) f = ciao_root_URL + "build/bin/" + path; else
          if (path.endsWith(".wasm")) f = ciao_root_URL + "build/bin/" + path; else
            if (path.endsWith(".bundle.js")) f = ciao_root_URL + "build/dist/" + path; else
              if (path.endsWith(".mods.data")) f = ciao_root_URL + "build/dist/" + path; else
                if (path.endsWith(".mods.js")) f = ciao_root_URL + "build/dist/" + path; else
                  // otherwise, use the default, the prefix (JS file's dir) + the path
                  f = prefix + path;
        return f;
      };
      Ciao.ciao_root_URL = ciao_root_URL;
      // Collect workspaces from dependencies
      let wksps = Ciao.collect_wksps();
      //
      EMCiao.preRun = [];
      EMCiao.onRuntimeInitialized = function() {
        Ciao.update_timestamps(wksps);
        //
        Ciao.bind_funcs();
        var bootfile = Ciao.bundle['core'].wksp + "/build/bin/" + Ciao.bootfile;
        Ciao.init(bootfile);
        /* Boot the engine and execute ciaowasm:main/0 (which exits with a
           live runtime) */
        Ciao.boot();
        onready();
      };
      EMCiao.print = onprintout; // capture the stdout
      EMCiao.printErr = onprinterr; // capture the stderr
      /* Initialize FS (global!) and preload files */
      EMCiao.preRun.push(function() {
        FS = EMCiao['FS'];
        // Preload bundle files (if needed)
        for (const b of Ciao.depends) {
          Ciao.preload_bundle(b);
        }
        var root_wksp = Ciao.bundle['core'].wksp;
        Ciao.preload_file(root_wksp, "build/bin/" + Ciao.bootfile); /* TODO: change wksp? */
        /* Set CIAOPATH from bundles */
        (EMCiao.getENV())['CIAOPATH'] = wksps.join(":");
      });
      /* Begin execution of Emscripten (wasm) compiled engine */
      CIAOENGINE.run(EMCiao);
    };

    /* --------------------------------------------------------------------------- */

    Ciao.bootfile='ciaowasm';

    /* Begin new query. See ciaowasm:query_one_fs/0 */
    Ciao.query_one_begin = function(goal) {
      var query;
      query = 'q(('+goal+')).';
      FS.writeFile('/.q-i', query, {encoding: 'utf8'});
      startTimer();
      Ciao.query_begin("ciaowasm:query_one_fs");
      return query_result();
    };

    /* Obtain next solution */
    Ciao.query_one_next = function() {
      startTimer();
      Ciao.query_next();
      return query_result();
    };

    /* Resume query */
    Ciao.query_one_resume = function() {
      Ciao.query_resume();
      return query_result();
    };

    function query_result() {
      var time = checkTimer();
      if (Ciao.query_ok()) {
        if (Ciao.query_suspended()) {
          return { cont: 'suspended', arg: '', time: time };
        } else {
          cont = FS.readFile('/.q-c', { encoding: 'utf8' });
          arg = FS.readFile('/.q-a', { encoding: 'utf8' });
          return { cont: cont, arg: arg, time: time };
        }
      } else {
        return { cont: 'failed', arg: '', time: time };
      }
    }

    /* --------------------------------------------------------------------------- */

    /* Get Emscripten FS */
    Ciao.getFS = function() { return FS; };

    /* --------------------------------------------------------------------------- */

    return Ciao;
  }());

  f.eng_load = function(url) {
    root_url = url;
    /* Load CIAOENGINE (generated from emcc) */
    importScripts(root_url + "build/bin/ciaoengwasm.js");
    /* connect with browser console (for stdout) */
    console=self.console; /* TODO: keep only in debug? */
    return true;
  }

  function monitor_deps(numdeps) {
    if (numdeps == 0) { /* There are no more pending loads */
      if (pending_load_hook !== undefined) { // send message
        pending_load_hook();
        pending_load_hook = undefined;
      }
      pending_load = false; // no pending loads
    }
  }
  f.use_bundle = function(bundle) {
    console.log(`{loading bundle '${bundle}'}`);
    var EMCiao = globalThis.__emciao;
    importScripts(root_url + "build/dist/" + bundle + ".bundle.js");
    if (Ciao.ciao_root_URL !== null) { /* not null if EMCiao is initialized */
      /* we can preload the bundle now */
      EMCiao['monitorRunDependencies'] = monitor_deps;
      pending_load = true; // at least one pending load
      Ciao.preload_bundle(bundle);
    }
    return true;
  }
  f.boot_info = function() {
    Ciao.run("internals:$bootversion"); // TODO: rename run by commit_call
    return true;
  }
  f.get_ciao_root = function() { return Ciao.bundle['core'].wksp; }
  f.get_stats = function() { return Ciao.stats; }
  f.writeFile = function(a, b) {
    try {
      return Ciao.getFS().writeFile(a, b, {encoding: 'utf8'});
    } catch(err) {
      return null;
    }
  }
  f.readFile = function(a) {
    try {
      return Ciao.getFS().readFile(a, {encoding: 'utf8'});
    } catch(err) {
      return null;
    }
  }
  f.read_stdout = function() {
    var out = stdout.replaceAll("\n$$$fake_flush$$$\n",""); /* TODO: see ciaowasm.pl for details about this horrible workaround */
    stdout = "";
    return out;
  }
  f.read_stderr = function() {
    var err = stderr.replaceAll("\n$$$fake_flush$$$\n",""); /* TODO: see ciaowasm.pl for details about this horrible workaround */
    stderr = "";
    return err;
  }
  // TODO: queue and send only 1?
  f.recv_jscmds = function() {
    let str;
    let FS = Ciao.getFS();
    try {
      str = FS.readFile('/.j-c', {encoding: 'utf8'});
      FS.unlink('/.j-c');
    } catch(err) {
      return [];
    }
    if (str === '') return [];
    let cmds = [];
    let lines = str.split("\n");
    for (let line of lines) {
      if (line === '') continue;
      cmds.push(JSON.parse(line));
    }
    return cmds;
  }
  f.send_jsret = function(x) {
    if (x === undefined) return true;
    try {
      let str = JSON.stringify(x);
      return Ciao.getFS().writeFile('/.j-o', str, {encoding: 'utf8'});
    } catch(err) {
      return null;
    }
  }
  f.query_one_begin = function(goal) { return Ciao.query_one_begin(goal); };
  f.query_one_resume = function() { return Ciao.query_one_resume(); };
  f.query_one_next = function() { return Ciao.query_one_next(); };
  f.query_end = function() { return Ciao.query_end(); };

  this.onmessage = function(event) {
    var resolve = (function (x) { postMessage({id: event.data.id, ret: x}); });
    switch(event.data.cmd) {
    case 'boot':
      /* (pass continuation) */
      Ciao.init_emciao(root_url, (function () {
        resolve(null);
      }), (function(out) {
        stdout += out + "\n";
      }), (function(err) {
        stderr += err + "\n";
      }));
      break;
    case 'wait_no_deps': /* Wait until there are no pending loading dependencies (for use_bundle) */
      if (pending_load) {
        if (pending_load_hook !== undefined) { throw new Error('unresolved pending load'); };
        pending_load_hook = (function() { resolve(true); }); // (send when ready)
      } else { /* answer immediately */
        resolve(true);
      }
      break;
    default:
      resolve(f[event.data.cmd].apply(undefined, event.data.args));
      break;
    }
  };

  /* Connect console */
  /* TODO: debug only? */
  console.log = function(msg) {
    postMessage({isLog: true, msg: msg});
    //  if (typeof msg == 'object') {
    //    logdiv.innerHTML += (JSON && JSON.stringify ? JSON.stringify(msg) : msg) + '<br/>';
    //  } else {
    //    logdiv.innerHTML += msg + '<br/>';
    //  }
  }
}

/* --------------------------------------------------------------------------- */

/**
 * A `CiaoWorker` is a instance of a Ciao engine running on a Web Worker
 */
class CiaoWorker {
  constructor(ciao_root_URL) {
    var listeners = [];
    this.eng_loaded = false;
    this.eng_booted = false;
    this.ciao_root_URL = ciao_root_URL;
    this.listeners = listeners;

    // Create Worker from string (better same-origin behaviour, specially in Chrome)
    //this.w = new Worker(ciao_root_URL + "build/bin/ciao-worker.js");
    let code = ciao_worker_fun.toString();
    code = code.slice(code.indexOf("{") + 1, code.lastIndexOf("}")); // get body (vars need to be global)
    const blob = new Blob([code], {type: 'text/javascript'});
    this.w = new Worker(URL.createObjectURL(blob)); // Note: use data-URI as an alternative "data:application/x-javascript;base64,"+...

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
  #async_(cmd, args) {
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
      cmd: cmd,
      args: args
    });
    return proxy;
  }

  /**
   * Ensure that Ciao is initialized. Level can be:
   *   1: engine loaded
   *   2: engine loaded and booted
   */
  async ensure_init(level) {
    if (this.pending_level >= level) return;
    this.pending_level = level;
    if (level >= 1 && !this.eng_loaded) {
      console.log(`{loading engine}`);
      this.eng_loaded = true;
      // (hack to get absolute url)
      let a = document.createElement('a');
      a.href = this.ciao_root_URL;
      let root_url = a.href; // TODO: needs to be absolute due to importScripts from Worker
      await this.#async_('eng_load', [root_url]);
    }
    if (level >= 2 && !this.eng_booted) {
      console.log(`{booting engine}`);
      this.eng_booted = true;
      await this.#async_('boot', []);
    }
  }

  /**
   * Use the bundle passed as a parameter, loading the engine if needed.
   * @param {string} name - Name of the bundle. 
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async use_bundle(name) {
    await this.ensure_init(1);
    return await this.#async_('use_bundle', [name]);
  };

  async wait_no_deps() {
    await this.ensure_init(1);
    return await this.#async_('wait_no_deps', []);
  }

  /**
   * Get the Ciao root path.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async get_ciao_root() {
    await this.ensure_init(1);
    return await this.#async_('get_ciao_root', []);
  };

  /**
   * Get the stats of the latest call.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async get_stats() {
    await this.ensure_init(1);
    return await this.#async_('get_stats', []);
  };

  /**
   * Show boot information in a JS console (this forces boot).
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async boot_info() {
    await this.ensure_init(2);
    return await this.#async_('boot_info', []);
  };

  /**
   * Begins the query passed as parameter and obtains one solution. The
   * decision tree stays awake and waits for user's input.
   * @param {string} goal - Query to be launched.
   * @return {Object} - Query result
   */
  async query_one_begin(goal) {
    await this.ensure_init(2);
    let q_out = await this.#async_('query_one_begin', [goal]);
    return await this.#query_complete(q_out);
  }

  /**
   * Obtain the next solution for the query previously launched.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async query_one_next() {
    let q_out = await this.#async_('query_one_next', []);
    return await this.#query_complete(q_out);
  }

  /* Resume query until completed (not suspended). Process jscmd buffer */
  async #query_complete(q_out) {
    while (true) {
      /* run cmds from jscmd buffer */
      let cmds = await this.#async_('recv_jscmds', []);
      for (let cmd of cmds) {
        let ret = await jscmd_run(this, cmd);
        await this.#async_('send_jsret', [ret]);
      }
      /* await to resume execution if we are in a suspended state */
      if (q_out.cont !== 'suspended') break;
      q_out = await this.#async_('query_one_resume', []); // (await here allows concurrency)
    }
    return q_out;
  }

  /**
   * End the query process stopping its decision tree.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async query_end() {
    return await this.#async_('query_end', []);
  }

  /**
   * Read file specified in path parameter.
   * @param {string} path - Path of the file to read.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async readFile(path) {
    await this.ensure_init(2);
    return await this.#async_('readFile', [path]);
  };

  /**
   * Write a string in a file.
   * @param {string} path - Path of the file to write.
   * @param {string} contents - String of the contents to write in the file.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async writeFile(path, contents) {
    await this.ensure_init(2);
    return await this.#async_('writeFile', [path, contents]);
  };

  /**
   * Capture the output of the most recent query/ies.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async read_stdout() {
    return await this.#async_('read_stdout', []);
  };

  /**
   * Capture the standard error of the most recent query/ies.
   * @return {CiaoPromiseProxy} - Result of the call to the worker.
   */
  async read_stderr() {
    return await this.#async_('read_stderr', []);
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

/* --------------------------------------------------------------------------- */
/* JS reference table */

// This table is used to assign unique indices to JS objects passed to
// a CiaoWorker.
//
// TODO:
//  - indices are reused but the heap table is not compacted
//  - use 'externref' [1] to simplify gluecode? (does it work from
//    workers?)
//  - foreign GC needed to do jsref_free automatically
//
// [1] https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md

/* The index->obj table (global and shared) */
var jsref_heap = []; // index->obj table
var jsref_freeidx = []; // free indices in the heap table (holes)

// Allocate a slot for `obj` and return its index
function jsref_alloc(obj) {
  if (jsref_freeidx.length != 0) { // reuse a free idx
    let idx = jsref_freeidx.pop();
    jsref_heap[idx] = obj;
    return idx;
  } else { // alloc a new idx
    return jsref_heap.push(obj) - 1; // index
  }
}

// Free the slot `idx`
function jsref_free(idx) {
  jsref_freeidx.push(idx); // add to the free pool
  jsref_heap[idx] = null; // remove
}

function jsref_obj(idx) {
  return jsref_heap[idx];
}

/* --------------------------------------------------------------------------- */
/* JS command buffer */

// JS command buffer is a JSON encoded list of commands.
//  
// Instructions are objects such that:
//
//   i.cmd=="def": assigns code i.code for function i.name at jscmd_f
//   i.cmd=="call": call function i.name with arguments i.args
//   i.cmd=="acall": await call function i.name with arguments i.args

// TODO: simplify this part using reference types
//   https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md

/* JS function table (global and shared) */
var jscmd_f = {};

/* Execute jscmd */
async function jscmd_run(w, jscmd) {
  let ret;
  switch(jscmd.cmd) {
  case 'def': /* define function */
    jscmd_f[jscmd.name] = Function('"use strict";return (' + jscmd.code + ')')();
    return null; /* TODO: provide id? */
  case 'call':
    jscmd.args.unshift(w); // worker as 1st argument
    return jscmd_f[jscmd.name].apply(undefined, jscmd.args);
  case 'acall':
    jscmd.args.unshift(w); // worker as 1st argument
    return await jscmd_f[jscmd.name].apply(undefined, jscmd.args);
  default:
    console.log('error: unknown jscmd: ' + jscmd);
  }
}
