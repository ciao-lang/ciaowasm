# Ciao Prolog from JavaScript

This bundle provides a Ciao engine compiled to WebAssembly
[Emscripten](https://emscripten.org/docs/getting_started/downloads.html)
and a JavsScript interface.

It provides:

 - A Ciao **engine** variant targeting WebAssembly.

 - The `wasm` **build grade** (see `src_builder/`), necessary to pack and
   distribute bundles for this backend.

 - A **high-level JS client** (`ciao-prolog.js`) to the Ciao engine,
   which is able to run queries and collect solutions. See
   `ciaowasm.pl` for internal details.

## Build

Install and enable
[Emscripten](https://emscripten.org/docs/getting_started/downloads.html)
SDK (EMSDK). Then use the `build.sh` script to prepare a build. Build
and installation will populate the `build/site/` directory at the
current workspace.

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
