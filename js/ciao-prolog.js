/*
 *  ciao-prolog.js
 *
 *  Main JavaScript interface to Ciao Prolog compiled to WebAssembly.
 *
 *  Copyright (C) 2017-2023 Jose F. Morales
 */

/**
   The JS interface for Ciao is implemented in several layers.  See
   the documentation of the following classes/objects for more
   information:

   - `ToplevelProc`
   - `CiaoWorker`
   - `LLCiao`
   - `CIAOENGINE`
*/

/* =========================================================================== */
/**
 * `CiaoPromiseProxy`: promise object used to store and communicate
 * results from `CiaoWorker` when using a Web Worker.
 */

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

/* =========================================================================== */
/**
 * `LLCiao`: Low level interface for CIAOENGINE. It implements
 * environment setup, loading of bundle data.
 *
 * This function should not have any dependency with the parent
 * code. It can be executed directly or captured as a string to be
 * loaded as a Web Worker.
 */

function new_LLCiao() {
  var ENVIRONMENT_IS_WORKER = typeof importScripts === 'function';
  var ENVIRONMENT_IS_NODE = typeof process == 'object' && typeof process.versions == 'object' && typeof process.versions.node == 'string';

  if (ENVIRONMENT_IS_NODE) {
    const fs = require('fs/promises');
    const vm = require('vm');
    var tryImportScript = async function(src) {
      global.require = require; // TODO: why?
      global.__dirname = __dirname; // TODO: why?
      global.tryImportScript = tryImportScript; // TODO: why?
      // TODO: better way?
      var data = await fs.readFile(src);
      // Note: emcc options "-s NODEJS_CATCH_EXIT=0 -s
      // NODEJS_CATCH_REJECTION=0" are required to prevent nodejs from
      // printing whole code on exceptions
      vm.runInThisContext(data, {displayErrors: false, filename:src});
    };
    var tryFetchJSON = async function(src) {
      var data = await fs.readFile(src);
      return JSON.parse(data);
    };
  } else {
    if (ENVIRONMENT_IS_WORKER) {
      var tryImportScript = async function(src) {
        importScripts(src); // async interface to importScripts from Web Worker
      };
    } else {
      var tryImportScript = globalThis.tryImportScript;
    }
    var tryFetchJSON = async function(src) {
      const response = await fetch(src);
      return await response.json();
    };
  }

  var EMCiao = {}; // Emscripten module for CIAOENGINE

  var stdout = "";
  var stderr = "";

  var LLCiao = {};
  LLCiao.emciao_initialized = false;
  LLCiao.stats = {};   // Statistics
  LLCiao.bundle = {};  // Bundle map
  LLCiao.depends = []; // Bundle dependencies
  LLCiao.root_URL = null; // URL for CIAOROOT (null when not initialized yet)

  /* --------------------------------------------------------------------------- */

  LLCiao.get_stats = function() { return LLCiao.stats; };

  /* --------------------------------------------------------------------------- */

  LLCiao.get_ciao_root = function() { return LLCiao.bundle['core'].wksp; };

  function mkdir_noerr(path) {
    let FS = LLCiao.getFS();
    try { FS.mkdir(path); } catch(e) { /* ignore errors */ };
  }

  // Create directory path
  function mkpath(path) {
    var spath = path.split('/');
    var len = spath.length;
    var dir = "";
    if (len > 0) {
      dir += spath[0];
      mkdir_noerr(dir);
      for (var i = 1; i < len; i++) {
        dir += "/"+spath[i];
        mkdir_noerr(dir);
      }
    }
  }

  LLCiao.preload_file = function(dir,relpath) {
    if (LLCiao.root_URL === null) throw new Error('null root_URL');
    var srcurl = LLCiao.root_URL + relpath;
    var srcdir = dir + '/' + relpath;
    /* Split in dir and base */
    var dirsplit = srcdir.split('/');
    var base = dirsplit.pop();
    var dir = dirsplit.join('/');
    /* Create directory path for dir */
    mkpath(dir);
    /* Create preloaded file */
    let FS = LLCiao.getFS();
    //FS.createPreloadedFile(dir, base, srcurl, true, false);
    FS.createPreloadedFile(dir, base, srcurl, true, true); // rw access
    // TODO: only works in web workers, does not work in lpdoc example
    // FS.createLazyFile(dir, base, srcurl, true, true); // rw access
  };

  async function maybeImportData(b_data, key) {
    if (b_data.hasOwnProperty(key)) {
      await tryImportScript(EMCiao.locateFile(b_data[key], ''));
    }
  }

  LLCiao.preload_bundle = async function(b) {
    // this loads *.mods.js
    globalThis.__emciao = EMCiao;
    const b_data = LLCiao.bundle[b];
    if (b_data.hasOwnProperty('preload_files')) {
      for (const rel_path of b_data.preload_files) {
        LLCiao.preload_file(b_data.wksp, rel_path);
      }
    }
    await Promise.all([maybeImportData(b_data, 'src_data'),
                       maybeImportData(b_data, 'mods_data')]);
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

  // Mount a directory (only for NodeJS)
  LLCiao.mount_dir = function(srcdir, dstdir) {
    mkpath(dstdir);
    var FS = LLCiao.getFS();
    FS.mount(FS.filesystems.NODEFS, { root: srcdir }, dstdir);
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
    if (!ENVIRONMENT_IS_NODE) console.log(`{loading bundle '${bundle}'}`);
    // Read and store .bundle.json metadata (see grade_wasm.pl)
    const b_data = await tryFetchJSON(LLCiao.root_URL + "build/dist/" + bundle + ".bundle.json");
    if (!LLCiao.depends) LLCiao.depends = [];
    LLCiao.depends.push(b_data.name);
    LLCiao.bundle[b_data.name] = b_data;
    // Initiate load if possible
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
    if (!ENVIRONMENT_IS_NODE) console.log(`{loading engine '${eng}'}`);
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
      // custom dirs
      let root_URL = LLCiao.root_URL;
      if (root_URL === null) throw new Error('null root_URL');
      if (path.endsWith(".mem")) return root_URL + "build/bin/" + path;
      if (path.endsWith(".wasm")) return root_URL + "build/bin/" + path;
      if (path.endsWith(".src.data")) return root_URL + "build/dist/" + path;
      if (path.endsWith(".src.js")) return root_URL + "build/dist/" + path;
      if (path.endsWith(".mods.data")) return root_URL + "build/dist/" + path;
      if (path.endsWith(".mods.js")) return root_URL + "build/dist/" + path;
      // otherwise, use the default, the prefix (JS file's dir) + the path
      return prefix + path;
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
      var bootfile = LLCiao.get_ciao_root() + "/build/bin/" + LLCiao.bootfile;
      let bootfile_ptr = EMCiao.stringToNewUTF8(bootfile);
      EMCiao._ciaowasm_init(bootfile_ptr);
      EMCiao._free(bootfile_ptr);
      /* Boot the engine, which will execute main/0 and exit with a live runtime */
      EMCiao._ciaowasm_boot();
      /* (Continue) */
      LLCiao.emciao_initialized = true;
      resolve(null);
    };
    /* Begin execution of Emscripten (wasm) compiled engine */
    if (!ENVIRONMENT_IS_NODE) console.log(`{booting engine}`);
    globalThis.CIAOENGINE.run(EMCiao);
  };

  /* --------------------------------------------------------------------------- */

  LLCiao.bootfile='ciaowasm';

  /* Begin new query. See ciaowasm:query_one_fs/0 */
  LLCiao.query_one_begin = function(goal) {
    var query;
    query = 'q(('+goal+')).';
    let FS = LLCiao.getFS();
    FS.writeFile('/.q-i', query, {encoding: 'utf8'});
    let q_ptr = EMCiao.stringToNewUTF8("ciaowasm:query_one_fs");
    EMCiao._ciaowasm_query_begin(q_ptr);
    EMCiao._free(q_ptr);
    return query_result();
  };

  /* Obtain next solution */
  LLCiao.query_one_next = function() {
    EMCiao._ciaowasm_query_next();
    return query_result();
  };

  /* Resume query */
  LLCiao.query_one_resume = function() {
    EMCiao._ciaowasm_query_resume();
    return query_result();
  };

  /* End query */
  LLCiao.query_end = function() {
    EMCiao._ciaowasm_query_end();
  }

  function recv_jscmd() {
    let str;
    let cmd;
    let FS = LLCiao.getFS();
    try {
      str = FS.readFile('/.j-c', {encoding: 'utf8'});
      FS.unlink('/.j-c');
    } catch(err) {
      return null;
    }
    return JSON.parse(str);
  }

  function query_result() {
    let is_ok = EMCiao._ciaowasm_query_ok();
    if (is_ok) {
      let is_suspended = EMCiao._ciaowasm_query_suspended();
      if (is_suspended) {
        let cmd = recv_jscmd(); // null if none
        // special case for debugger
        if (cmd !== null && cmd.cmd === 'acall' && cmd.name === '$dbgtrace_get_line') {
          return { cont: 'dbgtrace', arg: null };
        }
        return { cont: 'suspended', arg: cmd };
      } else {
        let FS = LLCiao.getFS();
        cont = FS.readFile('/.q-c', { encoding: 'utf8' });
        arg = FS.readFile('/.q-a', { encoding: 'utf8' });
        return { cont: cont, arg: arg };
      }
    } else {
      return { cont: 'failed', arg: '' };
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

/* =========================================================================== */

//var use_webworker = false;
var use_webworker = true;

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
      var url = this.root_URL;
      // (hack to get absolute url)
      if (use_webworker) {
        let a = document.createElement('a');
        a.href = url;
        url = a.href; // TODO: needs to be absolute due to importScripts from Web Worker
      }
      await this.#async_('eng_load', [url, "ciaoengwasm"]);
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

  /** Resume query until completed (not suspended). Process jscmd if needed
   */
  async #query_complete(q_out) {
    while (q_out.cont === 'suspended') {
      let cmd = q_out.arg;
      if (cmd !== null) { /* run cmd if needed */
        let ret = await jscmd_run(this, cmd);
        await this.#async_('send_jsret', [ret]);
      }
      q_out = await this.#async_('query_one_resume', []);
    }
    return q_out;
  }

  /** Resume a query suspended in dbgtrace
   */
  async query_resume_dbgtrace(dbgcmd) {
    /* pre: previous q_out.cont === 'dbgtrace' */
    await this.#async_('send_jsret', [dbgcmd]);
    let q_out = await this.#async_('query_one_resume', []);
    return await this.#query_complete(q_out);
  }

  /**
   * End the query.
   */
  async query_end() { return await this.#async_('query_end', []); }

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
  async read_stdout() { return await this.#async_('read_stdout', []); };

  /**
   * Capture the stderr of the most recent query.
   */
  async read_stderr() { return await this.#async_('read_stderr', []); };

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

/* =========================================================================== */
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

/* =========================================================================== */
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

/* =========================================================================== */
/**
 * `ToplevelProc`: toplevel (REPL) process
 */

// TODO: move more code to Prolog (this reimplements part of
//   toplevel.pl due to problems with blocking IO in
//   WASM/JS). Suspendable IO on wasm level would allow sharing more
//   code here.

const toplevelCfg_defaults = {
  // Show statistics (and some logging info) per query (in the JS console)
  statistics: true,
  // Query timeout (seconds) (0 to disable)
  query_timeout: 200,
  // Special queries // TODO: missing arity
  special_query: {
    "use_module": { read_code: true, mark_errs: true },
    "run_tests_in_module": {
      read_code: true,
      mark_errs: true,
      depends: ['ciaodbg'],
      on_init: ["use_module(library(unittest))"]
    },
//    "clean_mods": {
//      on_init: ['use_module(ciaobld(ciaoc_batch_call), [clean_mods/1])']
//    },
    "doc_cmd": {
      read_code: true,
      mark_errs: true,
      depends: ['lpdoc'],
      on_init: ['use_module(lpdoc(docmaker))']
    },
    //
    "set_menu_flag": { // arity {3}
      read_code: false,
      mark_errs: false,
      depends: ['ciaopp','typeslib'],
      on_init: ["use_module(ciaopp(ciaopp))"]
    },
    "module": {
      read_code: true,
      mark_errs: true,
      depends: ['ciaopp','typeslib'],
      on_init: ["use_module(ciaopp(ciaopp))"]
    },
    "auto_analyze": { // arity {1,2}
      read_code: true,
      mark_errs: true,
      depends: ['ciaopp','typeslib'],
      on_init: ["use_module(ciaopp(ciaopp))"]
    },
    "auto_optimize": { // arity {1,2}
      read_code: true,
      mark_errs: true,
      depends: ['ciaopp','typeslib'],
      on_init: ["use_module(ciaopp(ciaopp))"]
    },
    "auto_check_assert": { // arity {1,2}
      read_code: true,
      mark_errs: true,
      depends: ['ciaopp','typeslib'],
      on_init: ["use_module(ciaopp(ciaopp))"]
    },
    "filter_analyze": { // arity {1,2}
      read_code: true,
      mark_errs: true,
      depends: ['ciaopp','typeslib','exfilter'],
      on_init: ["use_module(exfilter(exfilter))"]
    },
    "filter_analyze_exercise_mode": { // arity {1,2}
      read_code: true,
      mark_errs: true,
      depends: ['ciaopp','typeslib','exfilter'],
      on_init: ["use_module(exfilter(exfilter))"]
    }
  },
  // Default bundles and initialization queries
  init_bundles: [
    'ciaowasm', // (for foreign-js)
    'core',
    'builder'
  ],
  init_queries: [
    'use_module(engine(internals), [reload_bundleregs/0])',
    'use_module(library(classic/classic_predicates))'
  ],
// Query for loading code
//   toplevelCfg.custom_load_query = ((m) => ...);
// Transformation for user queries
//   toplevelCfg.custom_run_query = ((q) => ... );
// Post-print code (before update_inner_layout()) 
//   toplevelCfg.custom_postprint_sol = (async (pg) => ...);
};

if (typeof toplevelCfg === 'undefined') { var toplevelCfg = {}; }
toplevelCfg = Object.assign({...toplevelCfg_defaults}, toplevelCfg);

var QueryState = {
  READY: 0, // ready for a query
  RUNNING: 1, // query running
  VALIDATING: 2, // prompt waiting for validating solution
  DBGTRACE: 3 // prompt in a debugging state
};

class ToplevelProc {
  constructor(root_URL) {
    this.root_URL = root_URL;
    this.w = null;
    this.comint = null; // associated comint ('null' to ignore)
    this.muted = false; // temporarily ignore comint // TODO: change comint instead?
    this.state = null;
    this.q_opts = {}; // running/validating query opts
    this.timer = undefined;
  }

  /* ---------------------------------------------------------------------- */

  /* Is the worker started? */
  is_started() {
    return (this.w !== null);
  }

  /* Start the worker (and load defaults, show prompt, load program) */
  async start() {
    if (!this.muted) this.comint.set_log('Loading bundles and booting');
    this.w = new CiaoWorker(this.root_URL); // create a Ciao worker
    await this.load_ciao_defaults(); // TODO: check result?
    if (!this.muted) this.comint.set_log(''); 
    //
    this.update_state(QueryState.READY);
    this.q_opts = {};
    if (!this.muted) this.comint.display_status_new_prompt('silent');
    await this.comint.pg.on_cproc_start();
  }

  /* Restart the worker */
  async restart() {
    this.shutdown();
    this.update_state(QueryState.READY);
    this.q_opts = {};
    const pmuted = this.set_muted(true); // TODO: mute on restart, make it optional?
    await this.start();
    this.muted = pmuted;
  }

  /* Terminate worker */
  shutdown() {
    if (!this.is_started()) return;
    this.w.terminate();
    this.w = null;
  }

  /* Make sure that worker is started */
  async ensure_started(comint) {
    if (this.is_started()) return;
    this.comint = comint; // attach to this comint
    await this.start();
  }

  // set muted and return previous value
  set_muted(v) {
    const prev = this.muted;
    this.muted = v;
    return prev;
  }

  /* ---------------------------------------------------------------------- */

  // Load the default bundles and modules
  async load_ciao_defaults() {
    // Use default bundles and show boot info (this starts the engine)
    for (const b of toplevelCfg.init_bundles) {
      await this.w.use_bundle(b);
    }
    // // TODO: parallel load does not speed up load
    // await Promise.all(toplevelCfg.init_bundles.map(async (b) => { await this.w.use_bundle(b); }));
    // Boot and show system info
    {
      this.w.curr_cproc = this; // TODO: simplify
      await this.w.query_one_begin("'$:'('internals:$bootversion')"); // TODO: check errors!
      let out = await this.w.read_stdout();
      let err = await this.w.read_stderr();
      await this.w.query_end();
      this.comint.pg.show_version(out+err);
    }
    // Initialization queries on the toplevel
    for (const q of toplevelCfg.init_queries) {
      await this.muted_query_dumpout(q);
    }
    return true;
  }

  // Add (and execute) a new initialization query 
  async push_on_init(qs) {
    const started = this.is_started();
    for (const q of qs) {
      if (!toplevelCfg.init_queries.includes(q)) {
        toplevelCfg.init_queries.push(q);
        if (started) await this.muted_query_dumpout(q);
      }
    }
  }

  // Add (and load) a new bundle dependency
  async push_depends(bs) {
    let updated = false;
    const started = this.is_started();
    for (const b of bs) {
      if (!toplevelCfg.init_bundles.includes(b)) {
        toplevelCfg.init_bundles.push(b);
        if (started) await this.w.use_bundle(b); // load if already started
        updated = true;
      }
    }
    // if (started && updated) await this.restart(); // TODO: not needed now!
    if (started && updated) {
      await this.w.wait_no_deps(); /* wait until there are no pending loading deps */
      await this.muted_query_dumpout('reload_bundleregs');
    }
  }

  /* ---------------------------------------------------------------------- */

  // Do a query, only one solution, dump stdout/stderr, 
  async muted_query_dumpout(q) {
    if (toplevelCfg.statistics) console.log(`{implicit: ${q}}`);
    this.w.curr_cproc = this; // TODO: simplify
    await this.w.query_one_begin(q);
    await this.dumpout(); // TODO: check errors!
    await this.w.query_end();
  }

  // Dump last query stdout/stderr (ignore or show in console)
  async dumpout() {
    let out = await this.w.read_stdout();
    let err = await this.w.read_stderr();
    if (toplevelCfg.statistics) console.log(out+err);
  }

  /* ---------------------------------------------------------------------- */

  set_query_timeout() {
    if (toplevelCfg.query_timeout == 0) return; /* no timeout */
    this.timer = setTimeout((async() => {
      if (!this.muted) this.comint.print_msg('\n{ABORTED: Time limit exceeded.}\n');
      await this.restart();
      if (!this.muted) this.comint.display_status_new_prompt('silent'); /* amend prompt if needed */
    }), toplevelCfg.query_timeout * 1000); /* set a timeout */
  }
  cancel_query_timeout() {
    if (this.timer !== undefined) {
      clearTimeout(this.timer);
      this.timer = undefined;
    }
  }

  /* ---------------------------------------------------------------------- */

  // TODO: only works for single goal queries; this needs to be done
  // at Prolog level with Prolog->JS communication

  async trans_query(query) {
    let treat_outerr = null;
    // apply transformation if needed
    if (toplevelCfg.custom_run_query !== undefined) {
      query = toplevelCfg.custom_run_query(query);
    }
    // perform special query actions
    let f_match = query.match(/([a-z][_a-zA-Z0-9]*)(?:\(|$)/); // functor name // TODO: arity is missing, do from Prolog
    if (f_match != null && f_match.length == 2) {
      const special_query = toplevelCfg.special_query[f_match[1]];
      if (special_query !== undefined) {
        if (special_query.depends !== undefined) { // new (bundle) dependencies
          await this.push_depends(special_query.depends);
        }
        if (special_query.on_init !== undefined) { // new initialization queries
          await this.push_on_init(special_query.on_init);
        }
        if (special_query.read_code === true) { // the query may read the code, upload to worker
          await this.comint.pg.upload_code_to_worker();
        }
        if (special_query.action !== undefined) { // replace auto_action
          this.comint.pg.set_auto_action(special_query.action);
        }
        if (special_query.mark_errs === true) { // the query may show messages on the code, treat outerr
          treat_outerr = async(out, err) => {
            this.comint.pg.mark_errs(out, err); // TODO: missing matching file?
          };
        }
      }
    }
    //
    return { q: query, treat_outerr: treat_outerr };
  }

  /* ---------------------------------------------------------------------- */

  update_state(state) {
    this.state = state;
    this.comint.update_inner_layout(); // (query state changed)
  }

  /* Alert if we are still running */
  check_not_running() {
    if (this.state === QueryState.RUNNING) {
      alert('Already running a query');
      return false;
    }
    return true;
  }
  /* Alert if we are locked validating/debugging in another comint */
  check_not_locked(comint) {
    if (this.comint !== comint) {
      alert('Already validating/debugging a query in other comint');
      return false;
    }
    return true;
  }
  /* Waiting for a line (validating query or debugging state) */
  is_waiting_for_line() {
    return (this.state === QueryState.VALIDATING || this.state === QueryState.DBGTRACE);
  }

  /**
   * Execute a new query on the toplevel (Pre: this.state === QueryState.READY)
   * @param {string} query - Query to be executed.
   */
  async run_query(comint, query, opts) {
    if (this.state !== QueryState.READY) {
      console.log('bug: already running or validating a query'); // TODO: treat_enter too fast?
      return; // TODO: query is lost!
    }
    this.comint = comint; // attach to this comint
    // ----
    let tr = await this.trans_query(query);
    query = tr.q;
    // TODO: almost duplicated
    this.update_state(QueryState.RUNNING);
    this.q_opts = opts;
    // begin a new query
    if (!this.muted && opts.msg !== undefined) this.comint.set_log(opts.msg); 
    this.set_query_timeout();
    this.w.curr_cproc = this; // TODO: simplify
    let q_out = await this.w.query_one_begin(query);
    this.cancel_query_timeout();
    if (!this.muted && opts.msg !== undefined) this.comint.set_log('');
    //
    await this.treat_sol(q_out, tr.treat_outerr); // treat query result
  }

  /**
   * Send a line command (`action`). This is used to validate
   * solutions (if this.state === QueryState.VALIDATING) or send a
   * debugger command (if this.state === QueryState.DBGTRACE)
   */
  async send_line(comint, action) {
    this.comint = comint; // attach to this comint
    if (this.state === QueryState.VALIDATING) {
      if (action === '') { // accept solution, end query
        this.w.curr_cproc = this; // TODO: simplify
        await this.w.query_end();
        this.update_state(QueryState.READY);
        this.q_opts = {};
        /*if (!this.muted)*/ this.comint.display_status_new_prompt('yes');
      } else { // ask for the next solution
        // TODO: almost duplicated
        this.update_state(QueryState.RUNNING);
        // next query solution
        this.set_query_timeout();
        this.w.curr_cproc = this; // TODO: simplify
        let q_out = await this.w.query_one_next();
        this.cancel_query_timeout();
        //
        await this.treat_sol(q_out, null); // treat query result
      }
    } else if (this.state === QueryState.DBGTRACE) {
      this.update_state(QueryState.RUNNING);
      // continue execution
      this.set_query_timeout();
      this.w.curr_cproc = this; // TODO: simplify
      let q_out = await this.w.query_resume_dbgtrace(action);
      this.cancel_query_timeout();
      //
      await this.treat_sol(q_out, null); // treat query result
    } else {
      console.log('bug: not in a validating/debugging solution state'); // TODO: treat_enter too fast?
    }
  }

  /**
   * If current query has a solution, print it and asks for more if
   * there are more solutions available. If it has no solutions, finish
   * the query.
   * @param {Object} q_out - Object containing an array with the solution of a query.
   */

  async treat_sol(q_out, treat_outerr) {
    let out = await this.w.read_stdout();
    let err = await this.w.read_stderr();
    /* print stdout and stderr output */
    if (!this.muted) this.comint.print_out(out+err);
    /* special case for debugger */
    if (q_out.cont === 'dbgtrace') { // TODO: better way?
      this.update_state(QueryState.DBGTRACE);
      return;
    }
    /* print solution */
    let solstatus;
    if (q_out.cont === 'failed') { // no more solutions
      solstatus = 'no';
    } else {
      // TODO: fixme, see toplevel.pl
      /* Pretty print query results (solutions or errors) */
      // (see ciaowasm.pl for possible cases)
      if (q_out.cont === 'success') {
        if (this.comint.with_prompt) { /* only if itr, otherwise ignore bindings and cut */
          let prettysol = q_out.arg;
          if (prettysol === '') { // (no bindings, cut)
            solstatus = 'yes';
          } else {
            solstatus = '?'; // TODO: not always! detect when there are no choicepoints
            if (!this.muted) this.comint.print_sol(prettysol); // print solution
          }
        }
      } else if (q_out.cont === 'exception') { // TODO: horrible hack
        let ball = q_out.arg;
        if (!this.muted) this.comint.print_msg(`{ERROR: No handle found for thrown exception ${ball}}\n`);
        solstatus = 'aborted';
      } else if (q_out.cont === 'malformed') {
        solstatus = 'silent';
        if (!this.muted) this.comint.print_msg('{SYNTAX ERROR: Malformed query}\n');
      } else {
        solstatus = 'silent';
        console.log(`bug: unrecognized query result cont: ${q_out.cont} ${q_out.arg}`);
      }
    }
    const no_treat_outerr = this.q_opts.no_treat_outerr;
    if (solstatus === '?') {
      this.update_state(QueryState.VALIDATING);
      if (!this.muted) this.comint.print_promptval();
    } else {
      await this.w.query_end();
      this.update_state(QueryState.READY);
      this.q_opts = {};
      if (!this.muted) this.comint.display_status_new_prompt(solstatus);
    }
    if (treat_outerr !== null) {
      if (no_treat_outerr !== true) {
        await treat_outerr(out, err);
      }
    } else if (!this.muted) {
      if (toplevelCfg.custom_postprint_sol !== undefined) {
        // custom postprint if needed
        await toplevelCfg.custom_postprint_sol(this.comint.pg);
      }
    }
  }

  /* ---------------------------------------------------------------------- */

  /**
   * Abort the execution of the current query (if inside of `run_query(query)`).
   */
  async abort() {
    if (this.state === QueryState.RUNNING) {
      this.cancel_query_timeout();
      // print message
      // if (!this.muted) this.comint.print_msg('\n{ Execution aborted }\n'); // (same text as Ciao)
      if (!this.muted) this.comint.print_msg('\n{ Execution aborted (resetting dynamic database) }\n'); // TODO: remove note when preserving the database is working
      // restart worker and update variables
      // TODO: just abort query, not the worker
      await this.restart();
      if (!this.muted) this.comint.display_status_new_prompt('silent'); /* amend prompt if needed */
    }
  }
}

/* =========================================================================== */
/**
 * Toplevel for execution under NodeJS
 */

// TODO: with_prompt is not configurable yet
// TODO: exit code is not returned

var ENVIRONMENT_IS_NODE = typeof process == 'object' && typeof process.versions == 'object' && typeof process.versions.node == 'string';

if (ENVIRONMENT_IS_NODE) {
  const path = require('path');
  const process = require('process');

  const printout = function(x) { process.stdout.write(x); };

  // Locate site path relative to the script dir
  var site_path = path.join(__dirname, '..');

  /* (nodejs) Readline-based Comint (for interacting with a ToplevelProc) */

  // Simpler PGCell for RLComint
  class RLPGCell {
    constructor(cproc) {
      this.cproc = cproc;
    }
    async setup(rl) {
      // readline-based comint
      this.comint = new RLComint(rl, this, {});
      // start
      await this.cproc.ensure_started(this.comint);
    }
    async on_cproc_start() {
    }
    show_version(str) {
      this.cproc.comint.display(str);
    }
    async upload_code_to_worker() {
    }
    mark_errs(out, err) {
    }
    /* mark source debug info */
    mark_srcdbg_info(info) {
      this.comint.display(`         In ${info.src} (${info.ln0}-${info.ln1}) ${info.pred}-${info.num}\n`);
    }
  }

  class RLComint {
    constructor(rl, pg, opts) {
      this.prompt = '?- ';
      this.promptval = ' ? ';

      this.with_prompt = ( opts.noprompt === true ? false : true ); // interactive
      this.pg = pg; // associated pgcell

      this.rl = rl; // readline
      this.next_prompt = ''; // prompt to be displayed (may contain solution)
    }

    update_inner_layout() {}
    display(text) { process.stdout.write(text); }

    print_out(str) { this.display(str); }
    print_sol(str) { this.next_prompt="\n"+str; }
    print_msg(str) { this.display(str); }

    print_prompt() {
      if (!this.with_prompt) return; /* (skip in non-interactive) */
      this.next_prompt = this.prompt;
    }
    print_promptval() {
      if (!this.with_prompt) return; /* (skip in non-interactive) */
      this.next_prompt += this.promptval; // (assume that print_sol has been called)
    }
    display_status(str) { // toplevel:display_status/1
      if (!this.with_prompt) return; /* (skip in non-interactive) */
      if (str === 'silent') return; /* skip this status */
      this.display('\n');
      this.display(str);
      this.display('\n');
    }
    display_status_new_prompt(str) { /* show status and a new prompt */
      this.display_status(str);
      this.print_prompt();
    }
    set_log(text) {}

    // TODO: refactor with ciao_playground.js
    async #treat_enter_(text) {
      const cproc = this.pg.cproc;
      if (cproc.is_waiting_for_line()) {
        if (!cproc.check_not_locked(this)) return; // TODO: not possible?
        await cproc.send_line(this, text);
      } else {
        // Perform query
        if (text === '') {
          this.print_prompt(); // show prompt again
        } else {
          if (text.slice(-1) !== '.') { // query is malformed
            // TODO: accept multiline inputs?
            this.display(`\
{SYNTAX ERROR: Malformed query. It must end with a period.}
`);
            this.display_status_new_prompt('silent');
          } else {
            let q = text.slice(0, -1);
            await cproc.run_query(this, q, {});
          }
        }
      }
    }

    /* Interaction loop */
    async loop() {
      const cproc = this.pg.cproc;
      let q_prompt = this.prompt;
      while(true) {
        let text;
        try {
          text = await this.rl.question(q_prompt);
        } catch(err) {
          console.log('Err: ' + Err);
          process.exit(1);
        }
        // TODO: do not capture 'halt.' here, set status from Prolog (so that exit code is properly notified)
        if (text.slice(-1) === '.' && text.slice(0, -1) === "halt") {
          break;
        }
        await this.#treat_enter_(text);
        q_prompt = this.next_prompt;
        this.next_prompt = "";
      }
    }
  }

  toplevelCfg.statistics = false;
  use_webworker = false;

  // Setup readline
  const readline = require('readline/promises');
  var is_terminal = process.stdout.isTTY && process.env.TERM !== 'dumb';
  let rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: is_terminal
  });
  rl.on('SIGINT', function() {
    console.log("Caught interrupt signal");
    process.exit(1);
  });
  rl.on('close', function() { // input ended
    process.exit(0);
  });

  (async () => {
    // Start a new toplevel and connect to a RLPGCell for interaction
    const cproc = new ToplevelProc(site_path+'/ciao/'); 
    var pg = new RLPGCell(cproc);
    await pg.setup(rl);
    
    // Mount current directory as /local
    // TODO: customize
    var currdir = process.cwd();
    var dstdir = '/local';
    cproc.w.llciao.mount_dir(currdir, dstdir);
    // Change directory
    // FS.chdir(dstdir); // TODO: FS.chdir does not seem to work, change from Prolog
    await cproc.muted_query_dumpout(`working_directory(_,'${dstdir}')`);
    // Enter loop
    await pg.comint.loop();
    // Exit
    rl.close();
  })();
}
