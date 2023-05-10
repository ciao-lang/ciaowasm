/*
 *  ciao-prolog.js
 *
 *  JavaScript interface to Ciao Prolog (wasm)
 *
 *  Copyright (C) 2017-2023 Jose F. Morales
 */

/**
   The JS interface for Ciao is implemented in several layers.  See
   the documentation of the following classes/objects for more
   information:

   - `CiaoWorker`
   - `LLCiao`
   - `CIAOENGINE`
*/

/* --------------------------------------------------------------------------- */

class CiaoPromiseProxy {
  /* TODO: similar to (:- block) implementation */
  /* TODO: 'reject' not supported */
  /* TODO: Make sure that this passes the Promises tests suite: https://github.com/promises-aplus/promises-tests */
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
 * `LLCiao`: Low level interface for CIAOENGINE. It implements
 * environment setup, loading of bundle data.
 *
 * This function should not have any dependency with the parent
 * code. It can be executed directly or captured as a string to be
 * loaded as a Web Worker.
 */

function new_LLCiao() {
  var EMCiao = {}; // Emscripten module for CIAOENGINE

  var stdout = "";
  var stderr = "";

  var prev_time = null;
  function now() { return (new Date()).getTime(); }
  function startTimer() { prev_time = now(); }
  function checkTimer() { return (now() - prev_time); } /* milliseconds */

  var LLCiao = {};
  LLCiao.emciao_initialized = false;
  LLCiao.stats = {};   // Statistics
  LLCiao.bundle = {};  // Bundle map
  LLCiao.depends = []; // Bundle dependencies
  LLCiao.root_URL = null; // URL for CIAOROOT (null when not initialized yet)

  /* Binding to C functions (see main-ciaowasm.c) */
  LLCiao.bind_funcs = function() {
    LLCiao.init = EMCiao.cwrap('ciaowasm_init', 'number', ['string']);
    LLCiao.boot = EMCiao.cwrap('ciaowasm_boot', 'number', []);
    LLCiao.query_begin = EMCiao.cwrap('ciaowasm_query_begin', 'number', ['string']);
    LLCiao.query_ok = EMCiao.cwrap('ciaowasm_query_ok', 'number', []);
    LLCiao.query_suspended = EMCiao.cwrap('ciaowasm_query_suspended', 'number', []);
    LLCiao.query_resume = EMCiao.cwrap('ciaowasm_query_resume', 'number', []);
    LLCiao.query_next = EMCiao.cwrap('ciaowasm_query_next', 'number', []);
    LLCiao.query_end = EMCiao.cwrap('ciaowasm_query_end', 'number', []);
  };

  /* --------------------------------------------------------------------------- */

  LLCiao.get_stats = function() { return LLCiao.stats; };

  /* --------------------------------------------------------------------------- */

  LLCiao.get_ciao_root = function() { return LLCiao.bundle['core'].wksp; };

  function mkdir_noerr(path) {
    let FS = LLCiao.getFS();
    try { FS.mkdir(path); } catch(e) { /* ignore errors */ };
  }

  LLCiao.preload_file = function(dir,relpath) {
    if (LLCiao.root_URL === null) throw new Error('null root_URL');
    var srcurl = LLCiao.root_URL + relpath;
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
    let FS = LLCiao.getFS();
    //FS.createPreloadedFile(dir, base, srcurl, true, false);
    FS.createPreloadedFile(dir, base, srcurl, true, true); // rw access
    // TODO: only works in web workers, does not work in lpdoc example
    // FS.createLazyFile(dir, base, srcurl, true, true); // rw access
  };

  LLCiao.preload_bundle = async function(b) {
    // this loads *.mods.js
    globalThis.__emciao = EMCiao;
    return LLCiao.bundle[b].preload();
  };

  LLCiao.collect_wksps = function() {
    let wksps = [];
    let seen = {};
    for (const j of LLCiao.depends) {
      var wksp = LLCiao.bundle[j].wksp;
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
  LLCiao.update_timestamps = function(wksps) {
    let FS = LLCiao.getFS();
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

  // Pending loads
  var pending_load = false;
  var pending_load_resolve = undefined;

  /* Suspend until all use_bundle has been completed */
  LLCiao.wait_no_deps = function() {
    return new Promise((resolve,reject) => { // TODO: 'reject' ignored
      if (pending_load) {
        if (pending_load_resolve !== undefined) { throw new Error('unresolved pending load'); };
        pending_load_resolve = (function() { resolve(true); }); // called from monitor_deps when done
      } else { /* there were no pending loads */
        resolve(true);
      }
    });
  };

  function monitor_deps(numdeps) {
    if (numdeps == 0) { // no more pending loads, notify if needed
      if (pending_load_resolve !== undefined) {
        pending_load_resolve();
        pending_load_resolve = undefined;
      }
      pending_load = false; // no pending loads
    }
  }
  LLCiao.use_bundle = async function(bundle) {
    console.log(`{loading bundle '${bundle}'}`);
    globalThis.__ciao = LLCiao; // for *.bundle.js
    // (this fill LLCiao.depends and LLCiao.bundle -- see grade_wasm.pl)
    await tryImportScript(LLCiao.root_URL + "build/dist/" + bundle + ".bundle.js");
    if (LLCiao.emciao_initialized) { /* (preRun has already been called, preload here) */
      pending_load = true; // at least one pending load
      await LLCiao.preload_bundle(bundle);
    }
    return true;
  }

  /* --------------------------------------------------------------------------- */
  /* Initialization */

  /** 
   * `CIAOENGINE` is the Emscripten/WASM compiled engine (very low
   * level). It is loaded dynamically from `eng` (e.g.,
   * `ciaoengwasm`).
   */

  LLCiao.eng_load = async function(url, eng) {
    LLCiao.root_URL = url;
    /* Load CIAOENGINE (generated from emcc) */
    console.log(`{loading engine '${eng}'}`);
    await tryImportScript(LLCiao.root_URL + "build/bin/" + eng + ".js");
    return true;
  };

  LLCiao.init_emciao = function() {
    return new Promise((resolve,reject) => { // TODO: 'reject' ignored
      LLCiao.init_emciao_(resolve);
    });
  };
  LLCiao.init_emciao_ = function(resolve) {
    /* Start the engine with hooks for initialization */
    EMCiao['locateFile'] = function(path, prefix) {
      var f;
      // custom dirs
      let root_URL = LLCiao.root_URL;
      if (root_URL === null) throw new Error('null root_URL');
      if (path.endsWith(".mem")) f = root_URL + "build/bin/" + path; else
        if (path.endsWith(".wasm")) f = root_URL + "build/bin/" + path; else
          if (path.endsWith(".bundle.js")) f = root_URL + "build/dist/" + path; else
            if (path.endsWith(".mods.data")) f = root_URL + "build/dist/" + path; else
              if (path.endsWith(".mods.js")) f = root_URL + "build/dist/" + path; else
                // otherwise, use the default, the prefix (JS file's dir) + the path
                f = prefix + path;
      return f;
    };
    // Collect workspaces from dependencies
    let wksps = LLCiao.collect_wksps();
    // Capture stdout and stderr
    EMCiao.print = function(out) {
      stdout += out + "\n";
    };
    EMCiao.printErr = function(err) {
      stderr += err + "\n";
    };
    // continue using code after run()
    EMCiao.noExitRuntime = true; 
    // preRun (setup environment before run())
    EMCiao.preRun = [];
    EMCiao.preRun.push(function() {
      // ('pre-js.js' intializes EMCiao['FS'] and EMCiao['getENV'])
      // Monitor run dependencies
      EMCiao['monitorRunDependencies'] = monitor_deps;
      // Preload bundle files (if needed)
      for (const b of LLCiao.depends) {
        // annotate asynchronous preload_bundle termination as run dependencies for emscripten
        var dep = "preload bundle "+b;
        EMCiao['addRunDependency'](dep);
        LLCiao.preload_bundle(b).then((result) => {
          EMCiao['removeRunDependency'](dep);
        });
      }
      // Preload bootfile so that it is accessible from the FS
      LLCiao.preload_file(LLCiao.get_ciao_root(), "build/bin/" + LLCiao.bootfile); /* TODO: customize */
      /* Set CIAOPATH from bundles */
      (EMCiao.getENV())['CIAOPATH'] = wksps.join(":");
    });
    // 
    EMCiao.onRuntimeInitialized = function() {
      LLCiao.update_timestamps(wksps);
      //
      LLCiao.bind_funcs();
      var bootfile = LLCiao.get_ciao_root() + "/build/bin/" + LLCiao.bootfile;
      LLCiao.init(bootfile);
      /* Boot the engine, which will execute main/0 and exit with a live runtime */
      LLCiao.boot();
      /* (Continue) */
      LLCiao.emciao_initialized = true;
      resolve(null);
    };
    /* Begin execution of Emscripten (wasm) compiled engine */
    console.log(`{booting engine}`);
    CIAOENGINE.run(EMCiao);
  };

  /* --------------------------------------------------------------------------- */

  LLCiao.bootfile='ciaowasm';

  /* Begin new query. See ciaowasm:query_one_fs/0 */
  LLCiao.query_one_begin = function(goal) {
    var query;
    query = 'q(('+goal+')).';
    let FS = LLCiao.getFS();
    FS.writeFile('/.q-i', query, {encoding: 'utf8'});
    startTimer();
    LLCiao.query_begin("ciaowasm:query_one_fs");
    return query_result();
  };

  /* Obtain next solution */
  LLCiao.query_one_next = function() {
    startTimer();
    LLCiao.query_next();
    return query_result();
  };

  /* Resume query */
  LLCiao.query_one_resume = function() {
    LLCiao.query_resume();
    return query_result();
  };

  function query_result() {
    var time = checkTimer();
    if (LLCiao.query_ok()) {
      if (LLCiao.query_suspended()) {
        return { cont: 'suspended', arg: '', time: time };
      } else {
        let FS = LLCiao.getFS();
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
  LLCiao.getFS = function() { return EMCiao['FS']; };

  LLCiao.read_stdout = function() {
    var out = stdout.replaceAll("\n$$$fake_flush$$$\n",""); /* TODO: see ciaowasm.pl for details about this horrible workaround */
    stdout = "";
    return out;
  };
  LLCiao.read_stderr = function() {
    var err = stderr.replaceAll("\n$$$fake_flush$$$\n",""); /* TODO: see ciaowasm.pl for details about this horrible workaround */
    stderr = "";
    return err;
  };

  LLCiao.writeFile = function(a, b) {
    try {
      return LLCiao.getFS().writeFile(a, b, {encoding: 'utf8'});
    } catch(err) {
      return null;
    }
  };
  LLCiao.readFile = function(a) {
    try {
      return LLCiao.getFS().readFile(a, {encoding: 'utf8'});
    } catch(err) {
      return null;
    }
  };

  /* --------------------------------------------------------------------------- */
  // TODO: queue and send only 1?

  LLCiao.recv_jscmds = function() {
    let str;
    let FS = LLCiao.getFS();
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
  };
  LLCiao.send_jsret = function(x) {
    if (x === undefined) return true;
    try {
      let str = JSON.stringify(x);
      return LLCiao.getFS().writeFile('/.j-o', str, {encoding: 'utf8'});
    } catch(err) {
      return null;
    }
  }

  // sync
  var f = {};
  f['get_stats'] = true;
  f['get_ciao_root'] = true;
  f['writeFile'] = true;
  f['readFile'] = true;
  f['read_stdout'] = true;
  f['read_stderr'] = true;
  f['recv_jscmds'] = true;
  f['send_jsret'] = true;
  f['query_one_begin'] = true;
  f['query_one_resume'] = true;
  f['query_one_next'] = true;
  f['query_end'] = true;
  // async
  var af={};
  af['eng_load'] = true;
  af['init_emciao'] = true;
  af['wait_no_deps'] = true;
  af['use_bundle'] = true;

  LLCiao.run_cmd = async function(cmd, args) {
    if (af.hasOwnProperty(cmd)) { // async
      return LLCiao[cmd].apply(undefined, args);
    } else if (f.hasOwnProperty(cmd)) { // sync
      return new Promise((resolve, reject) => { // TODO: 'reject' ignored
        resolve(LLCiao[cmd].apply(undefined, args));
      });
    } else {
      throw new Error("unknown cmd " + cmd);
    }
  };

  /* --------------------------------------------------------------------------- */

  return LLCiao;
}

// TODO: make Web Worker optional
function ciao_worker_fun() {
  var __ciao = new_LLCiao();
  var tryImportScript = async function(src) {
    importScripts(src); // async interface to importScripts from Web Worker
  };
  this.onmessage = function(event) {
    var resolve = (function (x) { postMessage({id: event.data.id, ret: x}); });
    __ciao.run_cmd(event.data.cmd, event.data.args).then(resolve);
  };
  /* connect with browser console */
  console=self.console; /* TODO: keep only in debug? */
}
// Note: use data-URI as an alternative "data:application/x-javascript;base64,"+...
function ciao_worker_url() {
  let code = new_LLCiao.toString();
  let worker_code = ciao_worker_fun.toString();
  worker_code = worker_code.slice(worker_code.indexOf("{") + 1, worker_code.lastIndexOf("}"));
  code += "\n" + worker_code; // get body (vars need to be global)
  const blob = new Blob([code], {type: 'text/javascript'});
  return URL.createObjectURL(blob);
}

/* --------------------------------------------------------------------------- */

//const use_webworker = false;
const use_webworker = true;

/**
 * `CiaoWorker`: High level interface, schedules JS<->Prolog
 * interaction, has (optional) Web Worker support (CiaoPromiseProxy).
 */
class CiaoWorker {
  constructor(root_URL) {
    this.eng_loaded = false;
    this.eng_booted = false;
    this.root_URL = root_URL;
    //
    if (use_webworker) {
      var listeners = [];
      this.listeners = listeners;
      this.w = new Worker(ciao_worker_url()); 
      this.w.onmessage = function(event) {
        listeners[event.data.id].setValue(event.data.ret);
        delete listeners[event.data.id]; /* undefine this array element */
      };
    } else {
      this.llciao = new_LLCiao();
    }
  }

  /**
   * Perform a request to `this.w` (the worker running the Ciao
   * engine). It creates and returns a CiaoPromiseProxy, whose value
   * is set on completion.
   */
  #async_(cmd, args) {
    if (use_webworker) {
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
    } else {
      return this.llciao.run_cmd(cmd, args);
    }
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
      this.eng_loaded = true;
      // (hack to get absolute url)
      let a = document.createElement('a');
      a.href = this.root_URL;
      let abs_root_URL = a.href; // TODO: needs to be absolute due to importScripts from Web Worker
      await this.#async_('eng_load', [abs_root_URL, "ciaoengwasm"]);
    }
    if (level >= 2 && !this.eng_booted) {
      this.eng_booted = true;
      await this.#async_('init_emciao', []);
    }
  }

  /**
   * Use the bundle passed as a parameter, loading the engine if needed.
   * @param {string} name - Name of the bundle. 
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
   * Get the stats of the latest call.
   */
  async get_stats() {
    await this.ensure_init(1);
    return await this.#async_('get_stats', []);
  };

  /**
   * Get the Ciao root path.
   */
  async get_ciao_root() {
    await this.ensure_init(1);
    return await this.#async_('get_ciao_root', []);
  };

  /**
   * Begins the query passed as parameter and obtains one solution. The
   * decision tree stays awake and waits for user's input.
   * @param {string} goal - Query to be launched.
   */
  async query_one_begin(goal) {
    await this.ensure_init(2);
    let q_out = await this.#async_('query_one_begin', [goal]);
    return await this.#query_complete(q_out);
  }

  /**
   * Obtain the next solution for the query previously launched.
   */
  async query_one_next() {
    let q_out = await this.#async_('query_one_next', []);
    return await this.#query_complete(q_out);
  }

  /** Resume query until completed (not suspended). Process jscmd buffer 
   */
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
   * End the query.
   */
  async query_end() {
    return await this.#async_('query_end', []);
  }

  /**
   * Read file specified in path parameter.
   * @param {string} path - Path of the file to read.
   */
  async readFile(path) {
    await this.ensure_init(2);
    return await this.#async_('readFile', [path]);
  };

  /**
   * Write a string in a file.
   * @param {string} path - Path of the file to write.
   * @param {string} contents - String of the contents to write in the file.
   */
  async writeFile(path, contents) {
    await this.ensure_init(2);
    return await this.#async_('writeFile', [path, contents]);
  };

  /**
   * Capture the stdout of the most recent query.
   */
  async read_stdout() {
    return await this.#async_('read_stdout', []);
  };

  /**
   * Capture the stderr of the most recent query.
   */
  async read_stderr() {
    return await this.#async_('read_stderr', []);
  };

  /**
   * Terminate the worker `w`
   */
  terminate() {
    if (use_webworker) {
      return this.w.terminate();
    } else {
      throw new Error('terminate needs Web Worker');
    }
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
