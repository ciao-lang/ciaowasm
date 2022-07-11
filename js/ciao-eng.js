/*
 *  ciao-eng.js
 *
 *  High-level interface to Ciao engine compiled to WebAssembly
 *
 *  Copyright (C) 2016-2022 Jose F. Morales
 */

/* This file contains a high-level interface to interact with the Wasm-compiled CIAOENGINE.
   It requires CIAOENGINE, which is automatically appended during the 'ciaowasm' build process.
 */

var Ciao = (function() {
  var Module = {}; // Engine compiled through Emscripten
  var FS; // Emscripten FS

  var prev_time = null;
  function now() { return (new Date()).getTime(); }
  function startTimer() { prev_time = now(); }
  function checkTimer() { return (now() - prev_time); } /* milliseconds */

  var Ciao = {};
  Ciao.stats = {};   // Statistics
  Ciao.bundle = {};  // Bundle map
  Ciao.depends = []; // Bundle dependencies
  Ciao.ciao_root_URL = ""; // URL for CIAOROOT

  Ciao.module = Module; // TODO: sure?

  Ciao.bind_funcs = function() {
    Ciao.init = Module.cwrap('ciaowasm_init', 'number', ['string']);
    Ciao.boot = Module.cwrap('ciaowasm_boot', 'number', []);
    Ciao.run = Module.cwrap('ciaowasm_run', 'number', ['string']);
    Ciao.query_begin = Module.cwrap('ciaowasm_query_begin', 'number', ['string']);
    Ciao.query_ok = Module.cwrap('ciaowasm_query_ok', 'number', []);
    Ciao.query_suspended = Module.cwrap('ciaowasm_query_suspended', 'number', []);
    Ciao.query_resume = Module.cwrap('ciaowasm_query_resume', 'number', []);
    Ciao.query_next = Module.cwrap('ciaowasm_query_next', 'number', []);
    Ciao.query_end = Module.cwrap('ciaowasm_query_end', 'number', []);
  };

  function mkdir_noerr(path) {
    try { FS.mkdir(path); } catch(e) { /* ignore errors */ };
  }

  Ciao.preload_file = function(dir,relpath) {
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

  /* --------------------------------------------------------------------------- */
  /* Initialization */

  Ciao.init = function(ciao_root_URL, onready, onprintout, onprinterr) {
    /* Start the engine with hooks for initialization */
    Module.noExitRuntime = true;
    Module['locateFile'] = function(path, prefix) {
      var f;
      // custom dirs
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
    // Collect workspaces
    let wksps = (() => {
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
    })();
    //
    Module.preRun = [];
    Module.onRuntimeInitialized = function() {
      // Update timestamps of build/cache files (around 8ms)
      // TODO: allow frozen workspaces in c_itf, inhibits recompilation!
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
      //
      Ciao.bind_funcs();
      startTimer();
      var bootfile = Ciao.bundle['core'].wksp + "/build/bin/" + Ciao.bootfile;
      Ciao.init(bootfile);
      /* Boot the engine and execute ciaowasm:main/0 (which exits with a
         live runtime) */
      Ciao.boot();
      Ciao.stats.runtime_boot = checkTimer();
      onready();
    };
    Module.print = onprintout; // capture the stdout
    Module.printErr = onprinterr; // capture the stderr
    /* Initialize FS (global!) and preload files */
    Module.preRun.push(function() {
      FS = Module['FS'];
      // Preload bundle files (if needed)
      for (const b of Ciao.depends) {
        Ciao.preload_bundle(b);
      }
      var root_wksp = Ciao.bundle['core'].wksp;
      Ciao.preload_file(root_wksp, "build/bin/" + Ciao.bootfile); /* TODO: change wksp?
      /* Set CIAOPATH from bundles */
      (Module.getENV())['CIAOPATH'] = wksps.join(":");
    });
    /* Begin execution of wasm compiled engine (this calls other hooks) */
    startTimer();
    Module.preRun.push(function() {
      Ciao.stats.runtime_engine_load = checkTimer();
    });
    CIAOENGINE.run(Module);
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
