/*
 *  ciao-worker.js
 *
 *  Wrapped for ciao-eng.js as Web Worker (see ciao-async.js)
 *
 *  Copyright (C) 2017-2022 Jose F. Morales
 */

var root_url;
var f={};
var stdout = "";
var stderr = "";
// Pending loads (status and msg id)
var pending_load = false;
var pending_load_id = undefined;

var Module;
var LZ4;

f.loadEng = function(url) {
  root_url = url;
  importScripts(root_url + "build/bin/ciao-eng.js");
  /* connect with browser console (for stdout) */
  console=self.console; /* TODO: keep only in debug? */
  return true;
}

function monitor_deps(numdeps) {
  if (numdeps == 0) {
    if (pending_load_id !== undefined) { // send message
      postMessage({id: pending_load_id, ret: true}); // use async
      pending_load_id = undefined;
    }
    pending_load = false; // no pending loads
  }
}

f.useBundle = function(bundle) {
  postMessage({isLog: true, msg: `{loading bundle '${bundle}'}`});
  importScripts(root_url + "build/dist/" + bundle + ".bundle.js");
  if (Module !== undefined) {
    Module['monitorRunDependencies'] = monitor_deps;
    pending_load = true; // at least one pending load
    Ciao.preload_bundle(bundle);
  }
  return true;
}

f.bootInfo = function() {
  Ciao.run("internals:$bootversion");
  console.log('[Engine loaded in '+
	      Ciao.stats.runtime_engine_load +
	      ' ms, boot in ' +
	      Ciao.stats.runtime_boot + ' ms]');
  return true;
}

f.get_ciao_root = function() {
  return Ciao.bundle['core'].wksp;
}

f.get_stats = function() {
  return Ciao.stats;
}

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

f.readStdout = function() {
  var out = stdout.replaceAll("\n$$$fake_flush$$$\n",""); /* TODO: see ciaowasm.pl for details about this horrible workaround */
  stdout = "";
  return out;
}

f.readStderr = function() {
  var err = stderr.replaceAll("\n$$$fake_flush$$$\n",""); /* TODO: see ciaowasm.pl for details about this horrible workaround */
  stderr = "";
  return err;
}

f.query = function(template, goal) {
  return Ciao.query(template, goal);
}

f.query_one_begin = function(template, goal) {
  return Ciao.query_one_begin(template, goal);
}

f.query_ok = function() {
  return Ciao.query_ok();
}

f.query_one_next = function() {
  return Ciao.query_one_next();
}

f.query_end = function() {
  return Ciao.query_end();
}

this.onmessage = function(event) {
  var ret = null;
  switch(event.data.cmd) {
  case 'boot':
    /* (pass continuation) */
    Ciao.init(root_url, (function () {
      postMessage({id: event.data.id, ret: ret});
    }), (function (out) {
      stdout += out + "\n";
    }), (function(err) {
      stderr += err + "\n";
    }));
    return;
  case 'waitNoDeps': /* Wait until there are no pending loading dependencies (for useBundle) */
    if (pending_load) {
      pending_load_id = event.data.id; /* wait */
    } else { /* answer immediately */
      postMessage({id: event.data.id, ret: ret});
    }
    return;
  default:
    ret = f[event.data.cmd].apply(undefined, event.data.args);
  }
  postMessage({id: event.data.id, ret: ret});
};

/* --------------------------------------------------------------------------- */
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
