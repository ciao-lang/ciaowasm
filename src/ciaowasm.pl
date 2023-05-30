:- module(ciaowasm, [], [assertions, doccomments]).

%! \title Boot code and support predicates for ciao-prolog.js
%
%  \module This defines a predicates to executes queries and returns
%  solutions using a minimalistic text-based interface.

:- doc(bug, "Allow creation of executables without main/{0,1}?").
:- doc(bug, "Allow suspended calls").
:- doc(bug, "Using FS for interaction, do it better?").
:- doc(bug, "Lower-level (not toplevel) interaction").

:- use_module(library(read), [read_term/3]).
:- use_module(library(streams)).
:- use_module(library(write)).
%
:- use_module(library(timeout), [call_with_time_limit/3]). % TODO: Currently not working, just add here to avoid a dynlink failure, e.g., from unittests

:- use_package(foreign_js).

% ---------------------------------------------------------------------------

:- use_module(engine(system_info), [get_arch/1]).

:- export(main/0).
main :-
    ( get_arch(wasm32) -> true
    ; display(user_error, 'Please do not call me directly! Use ciao-prolog.js\n\n')
    ),
    query_init.

:- export(query_one_fs/0).
% Execute a query and get solutions on backtracking
% (input and output from reserved files on file system)
query_one_fs :-
    get_query_fs(Query),
    ( Query = malformed -> write_sol(malformed)
    ; query_call_fs(Query)
    ).

% ---------------------------------------------------------------------------
% Execute queries

% :- compilation_fact(no_toplevel). % TODO: raw queries without whole toplevel dependencies, make it optional

:- if(defined(no_toplevel)).
:- use_module(library(compiler)). % allow use_module
:- use_module(engine(hiord_rt), [call/1]).
:- else.
:- use_module(engine(internals), ['$empty_gcdef_bin'/0]).
% TODO: avoid import, merge
:- import(toplevel, [shell_init/1, get_shell_module/1, query_call/4]).
:- endif.
:- use_module(library(toplevel/prettysols), [dump_solution/2, display_solution/1]).

:- if(defined(no_toplevel)).
% (raw queries, no toplevel)

query_init.

query_call_fs(q(Dict, VarNames, Goal)) :-
    query_(Goal, Dict, VarNames, Sol),
    write_sol(Sol).

% Execute `Goal` and get one solution (`success(Template)` or `exception(_)`)
query_(Goal, Dict, Sol) :-
    catch(port_query_(Goal, Dict, Sol), E, Sol=exception(E)), fake_flush.
query_(_, _, _) :- fake_flush, fail.

port_query_(Goal, Dict, success(Sol)) :-
    call(Goal),
    dump_solution(Dict, Sol). % TODO: lower-level

:- else.
% (full toplevel)

query_init :- toplevel:shell_init(['-q']). % TODO: make it customizable

% TODO: merge with toplevel:shell_query/2 and toplevel:valid_solution/3
query_call_fs(q(Dict, VarNames, Goal)) :-
    % TODO: unsafe? what if some thread is still running?
    '$empty_gcdef_bin', % Really get rid of abolished predicates
    toplevel:get_shell_module(ShMod),
    toplevel:query_call(Goal, ShMod, VarNames, Result),
    fake_flush,
    ( Result = yes(_) -> dump_solution(Dict, Sol), write_sol(success(Sol))
    ; Result = exception(_) -> write_sol(Result)
    ; Result = no -> fail
    ).

% (hook for toplevel_io.pl)
:- multifile top_get_line_hook/1.
top_get_line_hook(Line) :-
    fake_flush,
    js_call('$dbgtrace_get_line'(string(Line))).

% (foreign_js)
% [JS code ignored, treated directly in ciao-prolog.js]
js_def('$dbgtrace_get_line'("_"), [async, ret], "return null;").

% (hook for debugger_lib.pl)
:- multifile print_srcdbg_info_hook/6.
print_srcdbg_info_hook(Pport, Pred, Src, Ln0, Ln1, Number) :-
    fake_flush, % (only when printing to toplevel)
    atom_codes(Pport, PortCs),
    atom_codes(Pred, PredCs),
    atom_codes(Src, SrcCs),
    Info = json([port=string(PortCs),
                 pred=string(PredCs),
                 src=string(SrcCs),
                 ln0=Ln0,
                 ln1=Ln1,
                 num=Number]),
    js_call('$mark_srcdbg_info'(Info)).

% (foreign_js)
js_def('$mark_srcdbg_info'("info"), [], "w.curr_cproc.comint.pg.mark_srcdbg_info(info);").
    
:- endif.

% ---------------------------------------------------------------------------

% TODO: horrible hack: WASM Module.print and Module.printErr do not seem to send output until nl is found, any way to fix it?
fake_flush :-
    display(user_output, '\n$$$fake_flush$$$\n'),
    display(user_error, '\n$$$fake_flush$$$\n').

% ---------------------------------------------------------------------------
% FS interaction (rather than stdin/stdout/stderr)

% Get a query
get_query_fs(Query) :-
    open('/.q-i', read, In),
    Opts = [dictionary(Dict), variable_names(VarNames)],
    catch(read_term(In, Query0, Opts), _E, Err=yes),
    close(In),
    ( Err == yes ->
        Query = malformed
    ; Query0 = q(Goal) ->
        Query = q(Dict, VarNames, Goal)
    ; Query = malformed
    ).

% Write solutions
% TODO: enable faster parsing, avoid operator issues
write_sol(malformed) :- write_qc(malformed), write_qa('').
write_sol(success(X)) :- write_qc(success), write_qa_pretty(X).
write_sol(exception(X)) :- write_qc(exception), write_qa(X).

write_qc(X) :- open('/.q-c', write, Out), display(Out, X), close(Out).

write_qa(X) :- open('/.q-a', write, Out), writeq(Out, X), close(Out).

write_qa_pretty(Sol) :-
    open('/.q-a', write, Out),
    current_output(CurrOut), set_output(Out),
    ( Sol = [] -> true ; display_solution(Sol) ),
    set_output(CurrOut),
    close(Out).

