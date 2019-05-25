-module(user_default).
-export([l/0, nl/0, mm/0]).
-export([p/1, s/1]).
-export([help/0,dbgtc/1, dbgon/1, dbgon/2,
          dbgadd/1, dbgadd/2, dbgdel/1, dbgdel/2, dbgoff/0]).
-export(
   [load_conf/0,
    extend_paths/0]
  ).
%-compile(export_all).

-import(io, [format/1]).


load_conf() ->
    Conf = "./.econf",
    user_default_config:load_config(Conf).

extend_paths() ->    
    case user_default_config:get(ebin) of
	{found, Paths} ->
	    code:add_pathsa(Paths);
	not_found ->
	    []
    end.

%% @doc make in current dir (see also mk/0 for more flex)

mk0() ->
    io:format("~s\n", [os:cmd("make")]).

%% @doc make, then load all changed modules

mkl() ->
    mk0(),
    l().

%% mk/0 reads the directory to make first, then collects the modified
%% modules, loads them, and runs their EUnit tests (if any).
%%
%% Note: we need to run mm/0 before loading changed modules, since this
%%   set is changed by running the load.
%%
%% Note: we could run nl() instead of l(), but this way we confine all
%%  changes to the current node. On the other hand, we probably should
%%  ALWAYS use nl() when running distributed erlang, because it otherwise
%%  gets difficult to summarize where code is up-to-date.

mk() ->
    Dirs = config_value(),
    mk_dirs(Dirs).

mk_dirs(Dirs) ->
    lists:foreach(
      fun(Dir) ->
	      io:format("Make ~s\n", [Dir]),
	      mk(Dir)
      end,
      Dirs).

mk(Dir) ->
    io:format("~s\n", 
	      [os:cmd(
		 lists:flatten(
		   io_lib:format("(cd ~s && make)", [quote(Dir)])))]),
    Ms = mm(),
    l(),
    %% UNFINISHED
    %% - is this useful? not being used
    lists:foreach(
      fun(M) ->
	      case catch M:test() of
		  {'EXIT', {undef, _}} ->
		      %% skip modules w/o EUnit entry point
		      ok;
		  ok ->
		      %% (EUnit will normally print result)
		      ok;
		  Res ->
		      io:format("EUnit: ~p -> ~p\n", [M, Res])
	      end
      end,
      Ms).

%% UNFINISHED
%% - this should 

quote(Dir) ->
    Dir.

config_value() ->
    user_default_config:get(mkdir, []).

%% @doc Print entire value

p(X) ->
    io:format("~p\n", [X]).

%% @doc Print string (possibly io-list, so p/1 gets ugly)

s(Str) ->
    io:format("~s\n", [Str]).

%% @doc EUnit for module M.

u(M) ->
    case catch M:test() of
	{'EXIT', {undef, _}} ->
	    ok;
	Res ->
	    Res
    end.

%% @doc Load and unit-test all changed modules.

u() ->
    Ms = mm(),
    l(),
    lists:foreach(
      fun(M) ->
	      io:format("Unit testing ~p -> ~p\n", [M, u(M)])
      end,
      Ms
     ).
       
%% @doc Load all changed modules on current node.

l() ->
    lists:foreach(
      fun(M) ->
	      io:format("Loading ~p -> ~p~n", [M, c:l(M)])
      end,
      mm()
     ).

%% @doc Load all changed modules on all visible nodes
%%
%% UNFINISHED - does this load new code on slave nodes? if not, how
%%  do we do that?

nl() ->
    lists:foreach(
      fun(M) ->
	      io:format("Network loading ~p -> ~p~n", [M, c:nl(M)])
      end,
      mm()
     ).

%% Vladimir Sekissov's fine code

mm() ->
    modified_modules().

%% @doc Check changed modules on node N. We probably want this
%%  instead: compare mod date of modules on node N with mod date on
%%  current node [then do nl(M) if changed]

mm(Node) ->
    rpc:call(Node, ?MODULE, mm, []).

%% List of all modified modules.

modified_modules() ->
    code:modified_modules().

%% @doc Here is the actual work horse for OLD OTP. Note that this required
%% compile time as an attribute
%%
%% OBSOLETE
%% - we keep it around for older versions of OTP

old_modified_modules() ->
    [M || {M, _} <-  code:all_loaded(), module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
	{file, preloaded} ->
	    false;
	{file, Path} ->
	    CompileOpts = proplists:get_value(compile, Module:module_info()),
	    CompileTime = proplists:get_value(time, CompileOpts),
	    Src = proplists:get_value(source, CompileOpts),
	    module_modified(Path, CompileTime, Src);
	_ ->
	    false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
	false ->
	    false;
	ModPath ->
	    {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
	    CompileOpts =  binary_to_term(CB),
	    CompileTime = proplists:get_value(time, CompileOpts),
	    Src = proplists:get_value(source, CompileOpts),
	    not (CompileTime == PrevCompileTime) and (Src == PrevSrc)
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
	{ok, _} ->
	    Path;
	_ ->
	    %% maybe the path was changed?
	    %% NB: the following does not exist ... but the whole works
	    %% fairly well anyway
	    case code:where_is_file(filename:basename(Path)) of
		non_existing ->
		    false;
		NewPath ->
		    NewPath
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Run M:F(A1,...,An) on all visible nodes (perhaps just this one).

mc(M, F, As) ->
    Nodes = nodes(),
    case Nodes of
	[] ->
	    apply(M, F, As);
	_ ->
	    rpc:multicall(M, F, As)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% By "Serge Aleynikov" <serge@hq.idt.net>, erlang-questions Sep 2006

help() ->
    shell_default:help(),
    io:format("** user extended commands **~n"),
    io:format("dbgtc(File)   -- use dbg:trace_client() to read data from "
	      "File\n"),
    io:format("dbgon(M)      -- enable dbg tracer on all funs in module "
	      "M\n"),
    io:format("dbgon(M,Fun)  -- enable dbg tracer for module M and "
	      "function F\n"),
    io:format("dbgon(M,File) -- enable dbg tracer for module M and log to "
	      "File\n"),
    io:format("dbgadd(M)     -- enable call tracer for module M\n"),
    io:format("dbgadd(M,F)   -- enable call tracer for function M:F\n"),
    io:format("dbgdel(M)     -- disable call tracer for module M\n"),
    io:format("dbgdel(M,F)   -- disable call tracer for function M:F\n"),
    io:format("dbgoff()      -- disable dbg tracer (calls "
	      "dbg:stop/0)\n"),
    io:format("mk()          -- run make in configured dir, load, unit test\n"),
    io:format("mk(D)         -- run make in dir D, load, unit test\n"),
    io:format("mk0()         -- run make in current directory\n"),
    io:format("l()           -- load all changed modules\n"),
    io:format("nl()          -- load all changed modules on all known "
	      "nodes\n"),
    io:format("mm()          -- list modified modules\n"),
    true.

dbgtc(File) ->
    Fun = fun({trace,_,call,{M,F,A}}, _) ->
                 io:format("call: ~w:~w~w~n", [M,F,A]);
             ({trace,_,return_from,{M,F,A},R}, _) ->
                 io:format("retn: ~w:~w/~w -> ~w~n", [M,F,A,R]);
             (A,B) ->
                 io:format("~w: ~w~n", [A,B])
          end,
    dbg:trace_client(file, File, {Fun, []}).

dbgon(Module) ->
    case dbg:tracer() of
    {ok,_} ->
       dbg:p(all,call),
       dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
       ok;
    Else ->
       Else
    end.

dbgon(Module, Fun) when is_atom(Fun) ->
    {ok,_} = dbg:tracer(),
    dbg:p(all,call),
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]),
    ok;

dbgon(Module, File) when is_list(File) ->
    {ok,_} = dbg:tracer(file, dbg:trace_port(file, File)),
    dbg:p(all,call),
    dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
    ok.

dbgadd(Module) ->
    dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
    ok.

dbgadd(Module, Fun) ->
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]),
    ok.

dbgdel(Module) ->
    dbg:ctpl(Module),
    ok.

dbgdel(Module, Fun) ->
    dbg:ctpl(Module, Fun),
    ok.

dbgoff() ->
    dbg:stop().

