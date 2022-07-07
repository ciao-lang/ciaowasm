:- module(ciaowasm, [], [assertions, doccomments]).

%! \title Support predicates for ciao-eng.js
%
%  \module This defines a predicates to executes queries and returns
%  solutions using a minimalistic text-based interface.

:- doc(bug, "Allow creation of executables without main/{0,1}?").
:- doc(bug, "Allow suspended calls").
:- doc(bug, "Using FS for interaction, do it better?").

:- use_module(library(compiler)). % allow use_module
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
    write_sols([Sol]).

read_query(Query) :-
    open('/.ciaowasm-in.pl', read, In),
    Opts = [variable_names(Vs)],
    catch(read_term(In, Query0, Opts), _E, Err=yes),
    close(In),
    ( Err == yes ->
        Query = malformed
    ; Query0 = notmpl(Goal) -> % No template, use variable names
        Query = tmpl(Vs, Goal)
    ; Query0 = tmpl(_,_) ->
        Query = Query0
    ; Query = malformed
    ).

% Write one solution per line
write_sols(Sols) :-
    open('/.ciaowasm-out.pl', write, Out),
    ( member(Sol, Sols),
      write_sol(Sol, Out),
      fail
    ; true
    ),
    close(Out).

% TODO: enable faster parsing, avoid operator issues
write_sol(malformed, Out) :- display(Out, 'malformed'), nl(Out).
write_sol(success(X), Out) :- display(Out, 'success('), writeq(Out, X), display(Out, ')'), nl(Out).
write_sol(exception(X), Out) :- display(Out, 'exception('), writeq(Out, X), display(Out, ')'), nl(Out).

query_one(tmpl(Template, Goal), Sol) :-
    query_(Goal,Template,Sol).
query_one(malformed, Sol) :-
    Sol = malformed.

% Execute `Goal` and get one solution (`success(Template)` or `exception(_)`)
query_(Goal, Template, Sol) :-
    catch(port_query_(Goal, Template, Sol), E, Sol=exception(E)), fake_flush.
query_(_, _, _) :- fake_flush, fail.

% TODO: horrible hack: WASM Module.print and Module.printErr do not seem to send output until nl is found, any way to fix it?
fake_flush :-
    display(user_output, '\n$$$fake_flush$$$\n'),
    display(user_error, '\n$$$fake_flush$$$\n').

port_query_(Goal, Template, success(Template)) :- call(Goal).
