:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for CiaoWasm").

'$builder_hook'(cmd('ciaowasm', [main='src/ciaowasm', static])).

'$builder_hook'(item_nested(engwasm)).
'$builder_hook'(engwasm:eng('src/ciaoengwasm', [
  usepath(at_bundle(core, 'engine')),
  % cross('EMSCRIPTEN', wasm64) % TODO: allow both
  cross('EMSCRIPTEN', wasm32)
])).

'$builder_hook'(manual('ciaowasm', [main='doc/SETTINGS.pl'])).

'$builder_hook'(prepare_build_bin) :-
    check_emscripten.

:- use_module(library(format), [format/3]).
:- use_module(library(system), [find_executable/2]).

% TODO: find a better way? auto_install it if missing?
check_emscripten :-
    ( find_executable('emcc', _EMCC) -> true
    ; format(user_error,
"The command 'emcc' is missing in the PATH.~n"||
"~n"||
"Remember to install Emscripten SDK and make sure that your environment~n"||
"is updated (e.g., source emsdk_portable/emsdk_env.sh).~n"||
"~n", []),
      fail
    ).

