:- module(_, [], [dcg, datafacts]).

%! \title Foreign JS interface (runtime)
%
%  \stability alpha
%
%  \module This is the foreign JS interface for ciaowasm.

% TODO: optimizations
%  - pre-generate JS statically
%  - unfolding of JS calls, avoid expensive boxing/unboxing
%  - add js_tsx_begin, js_tsx_end to avoid intermediate $yield
% TODO: proxies for JS objects
%  - shared idx for JS objects
%  - explicit js_acquire and js_release for JS objects (allow GC)

:- use_module(library(stream_utils), [write_string/2, file_to_string/2]).
:- use_module(library(pillow/json), [json_to_string/2, string_to_json/2]).
:- use_module(library(streams)).
:- use_module(engine(internals), ['$yield'/0]).
:- use_module(library(lists), [member/2, append/3]).

:- data js_def_prop/3.

:- multifile js_def/3.
:- discontiguous(js_def/3).

% :- export(send_jscmd/1).
send_jscmd(Cmd) :-
    json_to_string(Cmd, Str),
    open('/.j-c', append, Out),
    write_string(Out, Str),
    close(Out),
    '$yield'. % (allow context switch to JS)

% Send definition dynamically
send_js_def(F, A) :-
    functor(Head, F, A),
    js_def(Head, Opts, Code),
    !,
    ( js_def_prop(F, A, sent) ->
        true % already sent
    ; % compose function
      Head =.. [Name|Args0],
      ( member(ret, Opts) ->
          assertz_fact(js_def_prop(F, A, ret)),
          Out = [_],
          ( append(In0, Out, Args0) -> true ; fail )
      ; In0 = Args0
      ),
      ( member(no_worker, Opts) ->
          assertz_fact(js_def_prop(F, A, no_worker)),
          In = In0
      ; In = ["w"|In0]
      ),
      ( member(async, Opts) ->
          assertz_fact(js_def_prop(F, A, async))
      ; true
      ),
      atom_codes(Name, NameCs),
      emit_fun(F, A, In, Code, Fun, []),
      % send definition
      send_jscmd(json([cmd=string("def"), name=string(NameCs), code=string(Fun)])),
      % mark as sent
      assertz_fact(js_def_prop(F, A, sent))
    ).

emit_fun(F, A, Args, Code) -->
    ( { js_def_prop(F, A, async) } ->
        "async"
    ; []
    ),
    "(", emit_args(Args), ") => { ", emit_str(Code), "}".

emit_args([]) --> [].
emit_args([A|As]) --> emit_arg(A), emit_args_(As).

emit_args_([]) --> [].
emit_args_([A|As]) --> ",", emit_arg(A), emit_args_(As).

emit_arg(X) --> emit_str(X).

emit_str(Str, S, S0) :- append(Str, S0, S).

% Call foreign JS
:- export(js_call/1).
js_call(Goal) :-
    functor(Goal, F, A),
    send_js_def(F, A),
    %
    Goal =.. [_|Args],
    ( js_def_prop(F, A, ret) ->
        Out = [_],
        ( append(In, Out, Args) -> true ; fail )
    ; Out = [], In = Args
    ),
    atom_codes(F, NameCs),
    ( js_def_prop(F, A, async) -> % async
        send_jscmd(json([cmd=string("acall"), name=string(NameCs), args=In]))
    ; send_jscmd(json([cmd=string("call"), name=string(NameCs), args=In]))
    ),
    ( Out = [Out1] -> % get output
        file_to_string('/.j-o', OutStr),
        string_to_json(OutStr, Out1)
    ; true
    ).

