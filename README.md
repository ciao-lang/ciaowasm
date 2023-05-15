# Ciao Prolog for JavaScript

This bundle provides a Ciao **engine** variant for WebAssembly
[Emscripten](https://emscripten.org/docs/getting_started/downloads.html),
and a **high-level JS client** (`ciao-prolog.js`). It also provides a
`wasm` **build grade** (see `src_builder/`), necessary to pack and
distribute bundles for this backend.

## Build and Usage

Install and enable
[Emscripten](https://emscripten.org/docs/getting_started/downloads.html)
SDK (EMSDK). Then use the `build.sh` script to prepare a build. Build
and installation will populate the `build/site/` directory in the
current workspace.

This interface is used in the **Ciao Playground** and **(LPdoc) Active
Logic Documents** to provide client-side Prolog execution in the
browser.

Additionally, experimental support for [Node.js](https://nodejs.org)
is available:
```
$ node build/site/js/ciao-prolog.js
Ciao 1.22.0 (2023-04-27 11:02:14 +0200) [EMSCRIPTENwasm32]
?- 
```
  
### Documentation of JavaScript bindings

Documentation of JavaScript bindings is auto-generated using
[JSDoc](https://jsdoc.app).

```
jsdoc -c Manifest/jsdoc-conf.json
```

The HTML documentation will be generated in the `doc/js/` folder.
Install `jsdoc` with `npm install -g jsdoc` (or `npm install
--save-dev jsdoc` locally).

## Caveats

 - Operations repending on blocking IO (like console interaction) are
   not handled nicely in
   Emscripten. [Asyncify](https://emscripten.org/docs/porting/asyncify.html)
   may be considered as an option but we must study its impact on
   performance (but it is written to be large and slow without `-O3`).
