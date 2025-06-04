:- module(ciaoengwasm, [], [assertions, library(compiler/emugen)]).

:- doc(title, "The Ciao Engine (wasm version)").
:- doc(author, "Ciao developers").

:- doc(module, "See @lib{ciaoengine} for more details about the Ciao
   engine. This variant is intended to be compiled with Emscripten (C
   to WebAssembly compiler)").

:- include(engine(ciaoengine_common)).
:- engine_stubmain('main-ciaowasm.c').

:- engine_opts([
  % (see ciaowasm.hooks.pl)
  % cross('EMSCRIPTEN', wasm64)
  % cross('EMSCRIPTEN', wasm32)
]).
