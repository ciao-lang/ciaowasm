:- module(ciaowasm, [], [assertions, doccomments]).

%! \title Support predicates for ciao-eng.js
%
%  \module This defines a predicates to executes queries and returns
%  solutions using a minimalistic text-based interface.

:- doc(bug, "Allow creation of executables without main/{0,1}?").
:- doc(bug, "Allow suspended calls").
:- doc(bug, "Using FS for interaction, do it better?").

:- use_module(library(compiler)). % allow use_module
:- use_module(library(toplevel/prettysols), [dump_solution/2, display_solution/1]).
:- use_module(library(read), [read_term/3]).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(lists), [member/2]).
:- use_module(engine(system_info), [get_arch/1]).
:- use_module(engine(hiord_rt), [call/1]).
%
:- use_module(library(timeout), [call_with_time_limit/3]). % TODO: Currently not working, just add here to avoid a dynlink failure, e.g., from unittests

:- export(main/0).
main :-
    ( get_arch(wasm32) -> true
    ; display(user_error, 'Please do not call me directly! I am the interface for ciao-eng.js\n\n')
    ).

:- export(query_one_fs/0).
% Execute a query and get solutions on backtracking
% (input and output from reserved files on file system)
query_one_fs :-
    read_query(Query),
    query_one(Query, Sol),
    write_sol(Sol).

read_query(Query) :-
    open('/.q-i', read, In),
    Opts = [dictionary(Dict)/*, variable_names(VarNames)*/],
    catch(read_term(In, Query0, Opts), _E, Err=yes),
    close(In),
    ( Err == yes ->
        Query = malformed
    ; Query0 = q(Goal) ->
        Query = q(Dict, Goal)
    ; Query = malformed
    ).

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

query_one(q(Dict, Goal), Sol) :-
    query_(Goal, Dict, Sol).
query_one(malformed, Sol) :-
    Sol = malformed.

% Execute `Goal` and get one solution (`success(Template)` or `exception(_)`)
query_(Goal, Dict, Sol) :-
    catch(port_query_(Goal, Dict, Sol), E, Sol=exception(E)), fake_flush.
query_(_, _, _) :- fake_flush, fail.

% TODO: horrible hack: WASM Module.print and Module.printErr do not seem to send output until nl is found, any way to fix it?
fake_flush :-
    display(user_output, '\n$$$fake_flush$$$\n'),
    display(user_error, '\n$$$fake_flush$$$\n').

port_query_(Goal, Dict, success(Sol)) :-
    call(Goal),
    dump_solution(Dict, Sol).
