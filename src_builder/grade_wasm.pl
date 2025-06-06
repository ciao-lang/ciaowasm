:- module(_, [], [fsyntax, hiord, doccomments, assertions, regtypes, isomodes, dcg]).

%! \title Binary grade for WebAssembly
%  \author Jose F. Morales
% 
%  \module This module implements the build grade for WebAssembly.
%    Compilation to WebAssembly is performed via Emscripten as a
%    cross-compilation target. This grade is needed to
%    perform the rest of actions required for this grade:
%
%    - composing an encapsulated engine (`ciaoengwasm.js`) from raw
%      WebAssembly compiled engine
%    - preparing bundle distributions for serving through HTTP
%    - computing meta-data for populating the Emscripten filesystem
%

:- use_module(ciaobld(builder_cmds), [builder_cmd/2, target_is_workspace/1]).
:- use_module(ciaobld(manifest_compiler), [target_is_bundle/1]).

% TODO: allow 'ciao-serve' to work on build directly (no install)
% TODO: implement 'clean' and 'uninstall'?
% TODO: installed bundles should be 'frozen' (no recompilation needed)
%       (this should speedup loading time considerably)

% ---------------------------------------------------------------------------

:- include(ciaobld(cmd_hooks)).

% % ---------------------------------------------------------------------------
% % build/clean (wasm)
% 
% 'grade.cmd'(wasm, build, build_wasm).
% 
% 'cmd.comment'(build_wasm, ["building [wasm]", "built [wasm]"]).
% 'cmd.grade'(build_wasm, wasm).
% 'cmd.needs_update_builder'(build_wasm).
% 'cmd.needs_rescan'(build_wasm).
% 'cmd.needs_config'(build_wasm).
% 'cmd.recursive'(build_wasm, forward).
% % TODO: temporary? move to install?
% 'cmd.do_after.decl'(build_wasm).
% 'cmd.do_after'(build_wasm, Target) :- !,
%       ( target_is_workspace(Target) -> true
%       ; target_is_bundle(Target) ->
%           dist_target(Target),
%           ( Target = core ->
%               dist_target('core.testsuite') % TODO: ad-hoc!
%           ; Target = ciaowasm ->
%               dist_target('ciaowasm.examples') % TODO: ad-hoc!
%           ; true
%           )
%       ; true
%       ).

% ---------------------------------------------------------------------------
% install/uninstall (wasm)

%K :- use_module(ciaobld(install_aux), [
%K   install_bin_dirs/1,
%K   uninstall_bin_dirs/1,
%K   install_bundlereg/1,
%K   uninstall_bundlereg/1
%K ]).

'grade.cmd'(wasm, install, install_wasm).

'cmd.comment'(install_wasm, ["installing [wasm]", "installed [wasm]"]).
'cmd.grade'(install_wasm, wasm).
%'cmd.only_global_instype'(install_wasm).
%'cmd.needs_update_builder'(install_wasm).
'cmd.needs_rescan'(install_wasm).
'cmd.recursive'(install_wasm, forward).
% TODO: temporary?
'cmd.do_after.decl'(install_wasm).
'cmd.do_after'(install_wasm, Target) :- !,
    ( target_is_workspace(Target) -> true
    ; target_is_bundle(Target) ->
        dist_bundle(Target)
    ; true
    ).

% ---------------------------------------------------------------------------
% Primitive targets for bin grade

:- use_module(library(lists), [member/2]).

% :- use_module(library(pathnames), [path_concat/3, path_split/3]).
% :- use_module(library(bundle/bundle_paths), [bundle_path/3]).
% :- use_module(ciaobld(messages_aux), [normal_message/2]).
% % (build)
% :- use_module(ciaobld(ciaoc_aux), [
%     build_eng_exec_header/1,
%     clean_eng_exec_header/1,
%     %
%     build_libs/2,
%     cmd_build/1
% ]).
% :- use_module(ciaobld(car_maker), [
%     eng_build/1,
%     eng_clean/1
% ]).
% % (installation)
% :- use_module(ciaobld(install_aux), [
%     eng_active_bld/1,
%     instdir_install/1,
%     instdir_uninstall/1,
%     inst_bundle_path/3,
%     final_ciao_root/1
% ]).

'grade.prim_kind'(wasm, bin) :- !.
'grade.prim_do'(wasm, Prim, Bundle, Cmd) :- !,
    prim(Prim, Bundle, Cmd).

% % eng/2: engines
% % TODO: mimic 'cmd'! (this is a very similar case)
prim(eng(_EngMainSpec, EngOpts), Bundle, install_wasm) :-
    % TODO: ad-hoc
    Bundle = ciaowasm, 
    member(cross('EMSCRIPTEN', wasm32), EngOpts), % (Emscripten)
    !,
    EngDef = eng_def(Bundle, _EngMainSpec, EngOpts),
    dist_engine(EngDef),
    site_copy_files. % TODO: move elsewhere?
prim(cmd(Path), Bundle, Cmd) :- atom(Path), !,
    % TODO: share
    path_split(Path, _, Name0),
    ( atom_concat(Name, '.pl', Name0) -> true
    ; Name = Name0
    ),
    prim(cmd(Name, [main=Path]), Bundle, Cmd).
prim(cmd(Name, _Opts), Bundle, install_wasm) :-
    % TODO: only those compatible? (e.g. not ciao-serve)
    !,
    ( Name = ciaowasm, Bundle = ciaowasm ->
        dist_cmd(Bundle, Name)
    ; true % TODO: ignore everything else at this moment
    ).
prim(_Prim, _Bundle, _Cmd) :-
    % (ignore others)
    % display(prim(_Prim, _Bundle, _Cmd)), nl.
    true.

% ---------------------------------------------------------------------------

:- use_module(ciaobld(config_common), [site_root_dir/1]).

% Target workspace for Emscripten output
distdir := ~path_concat(~site_root_dir, 'ciao'). % TODO: customize!

disttmpdir := ~bundle_path(core, builddir, 'sitetmp'). % TODO: customize!

% ---------------------------------------------------------------------------

:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(library(source_tree), [remove_dir/1]).
:- use_module(library(source_tree), [get_file_srctype/2]).
:- use_module(library(pathnames), [path_get_relative/3]).
:- use_module(library(bundle/bundle_paths), [bundle_workspace/2]).

bundle_dist_file_list(Bundle, Kind, RelPath) :-
    bundle_workspace(Bundle, Wksp),
    bundle_path(Bundle, '.', BaseDir),
    find_precomp_file(Kind, BaseDir, File),
    % workspace relative path
    path_get_relative(Wksp, File, RelPath).

% Files to be included in the wasm grade dist. 
% TODO: We should have a more fine-grained way to do this
check_dist_ext('.pl', src).
check_dist_ext('.po', src).
check_dist_ext('.itf', src).
% Source but also included as assets.
check_dist_ext('.js', src). % (assets)
check_dist_ext('.css', src). % (assets)
% Documentation files (.md and .lpdoc) included, e.g.  for the
% core/examples dir, if it contains .md examples
% TODO: add another kind for 'examples'?
check_dist_ext('.md', src). % (examples)
check_dist_ext('.lpdoc', src). % (examples)
%
check_dist_ext('.html', assets_http).
check_dist_ext('.js', assets_http).
check_dist_ext('.css', assets_http).
check_dist_ext('.png', assets_http).
check_dist_ext('.svg', assets_http).

:- use_module(engine(internals), [po_filename/2, itf_filename/2]).
:- use_module(engine(io_basic)).

% TODO: move or integrate into source_tree.pl library?
find_precomp_file(Kind, BaseDir, File) :-
    current_file_find(distributable_precomp(bin), BaseDir, File0),
    path_splitext(File0, Base, Ext),
    check_dist_ext(Ext, Kind),
    \+ check_nodist_file(BaseDir, File0, Ext), % do not distribute
    ( File = File0 % the file % TODO: pack .pl for modules in a different file
    ; % if File may be a module, try .po or .itf (po_filename/2 and itf_filename/2 works with CIAOCCACHE)
      ( Ext = '.pl' -> % maybe a module, try .po and .itf
        ( po_filename(Base, PO), file_exists(PO), File = PO
        ; itf_filename(Base, Itf), file_exists(Itf), File = Itf
        )
      ; fail
      )
    ).

% TODO: ad-hoc! customize
check_nodist_file(BaseDir, File, Ext) :-
    path_get_relative(BaseDir, File, RelFile),
    ( atom_concat('Manifest/', _, RelFile) -> true
    ; atom_concat('doc/', _, RelFile) -> true
    ; atom_concat('cmds/', _, RelFile) -> true
    ; atom_concat('src_builder/', _, RelFile) -> true
    ; ( Ext = '.md' ; Ext = '.lpdoc' ) ->
        % exclude docs from bundle root or source dirs (not examples)
        ( path_split(RelFile, '', _) -> true % bundle root
        ; atom_concat('engine/', _, RelFile) -> true % engine source
        ; atom_concat('src/', _, RelFile) -> true % src
        ; atom_concat('lib/', _, RelFile) -> true % lib
        ; atom_concat('library/', _, RelFile) -> true % library
        ; fail
        )
    ; fail
    ).

srcs(Bundle, RelPath) :-
    bundle_dist_file_list(Bundle, src, RelPath).

assets_http(Bundle, RelPath) :-
    bundle_dist_file_list(Bundle, assets_http, RelPath).

regs(Bundle, RelPath) :-
    ( RelPath = ~path_concat('build/bundlereg', ~atom_concat(Bundle, '.bundlereg'))
    ; RelPath = ~path_concat('build/bundlereg', ~atom_concat(Bundle, '.bundlecfg'))
    ).

% TODO: ad-hoc! customize
cachedata(ciaopp, Wksp, RelPath) :-
    CacheDir = 'build/data/ciaopp_lib_cache',
    ( X = 'lib_p_unit_db.pl'
    ; X = 'lib_typedb.pl'
    ),
    RelPath = ~path_concat(CacheDir, X),
    file_exists(~path_concat(Wksp, RelPath)). % (may not exist)

bundle_contents(Bundle, _, X) :- regs(Bundle, X).
bundle_contents(Bundle, Wksp, X) :- cachedata(Bundle, Wksp, X).
bundle_contents(Bundle, _, X) :- srcs(Bundle, X).

% ---------------------------------------------------------------------------

:- use_module(library(bundle/bundle_paths),
    [bundle_workspace/2, bundle_path/3, bundle_path/4]).
:- use_module(library(pathnames), [path_concat/3, path_split/3, path_splitext/3]).
:- use_module(ciaobld(eng_defs), [eng_mainmod/2, eng_path/3]).
:- use_module(ciaobld(config_common), [cmd_path/4]).

:- use_module(library(format), [format/3]).
:- use_module(library(system), [touch/1, copy_file/3, file_exists/1, cd/1, working_directory/2]).
:- use_module(library(system_extra), [mkpath/1, del_file_nofail/1]).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(aggregates), [findall/3]).

% ---------------------------------------------------------------------------
% Prepare a bundle distribution

:- use_module(library(pillow/json)).

% use_data_file :- fail.
use_data_file. % Store files in a single data file per bundle

% Prepare a bundle distribution and make it available in distdir
dist_bundle(Bundle) :-
    ToWksp = ~distdir,
    Wksp = ~bundle_workspace(Bundle),
    SrcDir = ~bundle_path(Bundle, '.'),
    dist_assets(Bundle, Wksp, ToWksp),
    dist_contents(Bundle, Wksp, ToWksp, Items, []),
    % Create .bundle.json file
    JSON = json([
        name = ~atmstr(Bundle),
        wksp = ~atmstr(Wksp),
        srcdir = ~atmstr(SrcDir)
    | Items]),
    Str = ~json_to_string(JSON),
    wksp_mkpath(ToWksp, 'build/dist'),
    BundleJS = ~rel_dist(ToWksp, ~path_concat('build/dist', ~atom_concat(Bundle, '.bundle.json'))),
    string_to_file(Str, BundleJS).

% Copy individual assets that need to be accessible through HTTP 
dist_assets(Bundle, Wksp, ToWksp) :-
    findall(X, assets_http(Bundle, X), Ys),
    wksp_copy_files(Ys, copy, Wksp, ToWksp).

% Copy bundle contents
dist_contents(Bundle, Wksp, ToWksp, Items, Items0) :-
    findall(X, bundle_contents(Bundle, Wksp, X), Xs),
    ( use_data_file ->
        split_dist(Xs, Wksp, Srcs, Mods),
        ( bundle_need_src(Bundle) -> Mode = copy ; Mode = touch ),
        dist_data_sect(src_data, ~atom_concat(Bundle, '.src'), Srcs, Mode, Wksp, ToWksp, Items, Items1),
        dist_data_sect(mods_data, ~atom_concat(Bundle, '.mods'), Mods, copy, Wksp, ToWksp, Items1, Items0)
    ; % TODO: allow both
      wksp_copy_files(Xs, copy, Wksp, ToWksp),
      Items = [preload_files = ~atmstr_list(Xs)|Items0]
    ).

% TODO: customize! generate both and load dynamically
%bundle_need_src(core).
bundle_need_src(_). % TODO: it seems to work, but it needs more testing

dist_data_sect(DataKey, DataItem, Xs, Mode, Wksp, ToWksp, Items, Items0) :-
    ( Xs = [] ->
        Items = Items0
    ; Items = [DataKey = ~atmstr(~atom_concat(DataItem, '.js'))|Items0],
      wksp_mkpath(ToWksp, 'build/dist'),
      wksp_pack_files(Xs, Mode, Wksp, ToWksp, ~path_concat('build/dist', DataItem))
    ).

% split files into sources (e.g., for analysis) and packages/incldued/precompiled (already compiled)
split_dist([], _, [], []).
split_dist([X|Xs], Wksp, Srcs, Mods) :-
    ( path_splitext(X, _, '.pl'),
      path_concat(Wksp, X, Path),
      get_file_srctype(Path, module) ->
        Srcs = [X|Srcs0], Mods = Mods0 % source
    ; Srcs = Srcs0, Mods = [X|Mods0] % other
    ),
    split_dist(Xs, Wksp, Srcs0, Mods0).

wksp_copy_files([], _Mode, _FromWksp, _ToWksp).
wksp_copy_files([X|Xs], Mode, FromWksp, ToWksp) :-
    wksp_copy_file_ensure_path(Mode, FromWksp, ToWksp, X),
    wksp_copy_files(Xs, Mode, FromWksp, ToWksp).

wksp_copy_file_ensure_path(Mode, FromWksp, ToWksp, X) :-
    path_split(X, D, _),
    wksp_mkpath(ToWksp, D),
    wksp_copy_file(Mode, FromWksp, ToWksp, X).

% Path is the full path given Wksp and relative path RelPath
rel_dist(Wksp, RelPath, Path) :-
    ( RelPath = '.' -> Path = Wksp
    ; Path = ~path_concat(Wksp, RelPath)
    ).

wksp_mkpath(ToWksp, Path) :- mkpath(~rel_dist(ToWksp, Path)).

wksp_copy_file(Mode, FromWksp, ToWksp, RelPath) :-
    From = ~path_concat(FromWksp, RelPath),
    To = ~rel_dist(ToWksp, RelPath),
    ( From == To -> throw(bug_copy_same_wksp) % copy_file/3 erases content!
    ; ( Mode = copy -> copy_file(From, To, [overwrite])
      ; Mode = touch -> touch(To) % (just an empty file)
      ; throw(bug)
      )
    ).

wksp_pack_files(Xs, Mode, Wksp, ToWksp, Name) :-
    DataFile = ~path_concat(ToWksp, ~atom_concat(Name, '.data')),
    JSFile = ~path_concat(ToWksp, ~atom_concat(Name, '.js')),
    ( Xs = [] -> % no files to pack, remove output
        del_file_nofail(DataFile),
        del_file_nofail(JSFile)
    ; TmpWksp = ~disttmpdir,
      remove_dir_if_exists(TmpWksp),
      mkpath(TmpWksp),
      wksp_copy_files(Xs, Mode, Wksp, TmpWksp),
      pack_wksp(Wksp, TmpWksp, DataFile, JSFile),
      remove_dir_if_exists(TmpWksp)
    ).

remove_dir_if_exists(X) :-
    ( file_exists(X) -> remove_dir(X) ; true ).

atmstr(X) := string(Cs) :- atom_codes(X,Cs).

atmstr_list([]) := [].
atmstr_list([X|Xs]) := [~atmstr(X) | ~atmstr_list(Xs)].

% ---------------------------------------------------------------------------

rel_bin_dir := 'build/bin'.

% Put together ciaoengwasm.js and the WASM compiled engine
dist_engine(Eng) :-
    ObjDir = ~eng_path(objdir, Eng),
    EngName = ~eng_mainmod(Eng),
    EngJs = ~atom_concat(EngName, '.js'),
%       EngJsMem = ~atom_concat(EngJs, '.mem'), % (not in WASM)
    EngWasm = ~atom_concat(EngName, '.wasm'),
    %
    DistBinDir = ~rel_dist(~distdir, ~rel_bin_dir),
    mkpath(DistBinDir),
    % OutJs = 'build/eng/ciaoengwasm/objs/ciaoengwasm.js',
    OutJs = ~path_concat(DistBinDir, 'ciaoengwasm.js'),
    copy_file(~bundle_path(ciaowasm, 'js/pre-js.js'), OutJs, [overwrite]),
    copy_file(~path_concat(ObjDir, EngJs), OutJs, [append]),
    copy_file(~bundle_path(ciaowasm, 'js/post-js.js'), OutJs, [append]),
%       % Copy the engine .js.mem
%       copy_file(~path_concat(ObjDir, EngJsMem), ~path_concat(DistBinDir, EngJsMem), [overwrite]),
    % Copy the engine .wasm
    copy_file(~path_concat(ObjDir, EngWasm), ~path_concat(DistBinDir, EngWasm), [overwrite]).

site_copy_files :-
    % Copy JS client
    SiteJs = ~path_concat(~site_root_dir, 'js'),
    mkpath(SiteJs),
    copy_file(~bundle_path(ciaowasm, 'js/ciao-prolog.js'), ~path_concat(SiteJs, 'ciao-prolog.js'), [overwrite]).

% ---------------------------------------------------------------------------

dist_cmd(Bundle, Name) :-
    cmd_path(Bundle, plexe, Name, Exe),
    bundle_workspace(Bundle, Wksp),
    path_get_relative(Wksp, Exe, RelPath),
    wksp_copy_file_ensure_path(copy, Wksp, ~distdir, RelPath).

% ---------------------------------------------------------------------------
% Call Emscripten's file_packager.py to generate compressed LZ4 data files

:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [find_executable/2, file_property/2]).
:- use_module(engine(stream_basic), [fixed_absolute_file_name/3]).

% Pack files from FromDir (which will be mounted on FinalDir). Output DataFile and JSFile.
pack_wksp(FinalDir, FromDir, DataFile, JSFile) :-
    emcc_file_packager([
      DataFile,
      '--lz4',
      % '--use-preload-cache', % (limits?)
      '--export-name=globalThis.__emciao',
      '--preload',
      ~atom_concat(FromDir, ~atom_concat('@', FinalDir)),
      ~atom_concat('--js-output=', JSFile)]).

find_file_packager(Exec) :-
    ( find_executable('emcc', EMCC0) -> true
    ; format(user_error, "ERROR: emscripten not found in path!~n", []),
      fail
    ),
    ( file_property(EMCC0, linkto(Rel)) ->
        path_split(EMCC0, EMDir0, _),
        fixed_absolute_file_name(Rel, EMDir0, EMCC)
    ; EMCC = EMCC0
    ),
    path_split(EMCC, EMDir, _),
    ( possible_rel_path(RelPath),
      path_concat(EMDir, RelPath, Exec),
      file_exists(Exec) -> true
    ; format(user_error, "ERROR: emscripten's file_packager.py not found!~n", []),
      fail
    ).

possible_rel_path('../libexec/tools/file_packager.py').
possible_rel_path('tools/file_packager.py').

emcc_file_packager(Args) :-
    find_file_packager(Exec),
    % process_call(path(python), [Exec|Args], []).
    process_call(Exec, Args, []).

