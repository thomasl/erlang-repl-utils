-module(user_default).
-moduledoc """
Extra commands for the Erlang shell.

The shell looks in this module for commands that it does not know, so
each exported function can be called without the module name.  Type
`help().` in the shell for a summary.

Configuration (the directories to run `make` in and the directories to
add to the code path) is read from the file `.econf` in the directory
where Erlang was started.  See `m:user_default_config`.
""".

%% Start-up (called from ~/.erlang)
-export([load_conf/0, extend_paths/0]).
%% Build, load and test
-export([mk/0, mk/1, mk0/0, mkl/0,
         l/0, nl/0, mm/0, mm/1,
         u/0, u/1]).
%% Printing
-export([p/1, s/1]).
%% Distribution
-export([mc/3]).
%% Tracing
-export([dbgtc/1, dbgon/1, dbgon/2, dbgadd/1, dbgadd/2,
         dbgdel/1, dbgdel/2, dbgoff/0]).
-export([help/0]).

-define(CONFIG_FILE, ".econf").

%% Match spec: trace the return value of each traced call.
-define(RETURN_TRACE, [{'_', [], [{return_trace}]}]).

%%%-------------------------------------------------------------------
%%% Start-up
%%%-------------------------------------------------------------------

-doc "Load the configuration file `.econf` from the current directory.".
-spec load_conf() -> ok | {ok, {already_loaded, file:name_all()}}.
load_conf() ->
    user_default_config:load_config(?CONFIG_FILE).

-doc "Add the directories of the config key `ebin` to the code path.".
-spec extend_paths() -> ok | [file:filename()].
extend_paths() ->
    case user_default_config:get(ebin) of
        {found, Paths} -> code:add_pathsa(Paths);
        not_found -> []
    end.

%%%-------------------------------------------------------------------
%%% Build, load and test
%%%-------------------------------------------------------------------

-doc """
Run `make` in each directory of the config key `mkdir`.  After each
make, load the changed modules and run their EUnit tests.
""".
-spec mk() -> ok.
mk() ->
    lists:foreach(
      fun(Dir) ->
              io:format("Make ~ts\n", [Dir]),
              mk(Dir)
      end,
      user_default_config:get(mkdir, [])).

-doc """
Run `make` in the directory `Dir`, then load the changed modules and
run their EUnit tests.
""".
-spec mk(file:filename()) -> ok.
mk(Dir) ->
    s(os:cmd("cd " ++ shell_quote(Dir) ++ " && make")),
    u().

-doc "Run `make` in the current directory.".
-spec mk0() -> ok.
mk0() ->
    s(os:cmd("make")).

-doc "Run `make` in the current directory, then load the changed modules.".
-spec mkl() -> ok.
mkl() ->
    mk0(),
    l().

-doc """
Load all changed modules on the current node.

Only the current node changes.  Use `nl/0` on a distributed system, so
that all nodes run the same code.
""".
-spec l() -> ok.
l() ->
    lists:foreach(
      fun(M) -> io:format("Loading ~p -> ~p\n", [M, c:l(M)]) end,
      mm()).

-doc "Load all changed modules on all connected nodes.".
-spec nl() -> ok.
nl() ->
    lists:foreach(
      fun(M) -> io:format("Network loading ~p -> ~p\n", [M, c:nl(M)]) end,
      mm()).

-doc """
List the changed modules: the loaded modules whose beam file on disk
is different from the loaded code.
""".
-spec mm() -> [module()].
mm() ->
    code:modified_modules().

-doc "List the changed modules on the node `Node`.".
-spec mm(node()) -> [module()].
mm(Node) ->
    erpc:call(Node, code, modified_modules, []).

-doc "Load all changed modules, then run their EUnit tests.".
-spec u() -> ok.
u() ->
    %% Get the list first: a module is not "changed" after the load.
    Ms = mm(),
    l(),
    lists:foreach(
      fun(M) ->
              case has_tests(M) of
                  true -> io:format("Unit testing ~p -> ~p\n", [M, M:test()]);
                  false -> ok
              end
      end,
      Ms).

-doc """
Run the EUnit tests of the module `M`.  Do nothing if `M` does not
export `test/0`.
""".
-spec u(module()) -> term().
u(M) ->
    case has_tests(M) of
        true -> M:test();
        false -> ok
    end.

has_tests(M) ->
    _ = code:ensure_loaded(M),
    erlang:function_exported(M, test, 0).

%% Quote a string for /bin/sh.
shell_quote(Str) ->
    "'" ++ string:replace(Str, "'", "'\\''", all) ++ "'".

%%%-------------------------------------------------------------------
%%% Printing
%%%-------------------------------------------------------------------

-doc "Print the full value of `X`.  The shell abbreviates large values.".
-spec p(term()) -> ok.
p(X) ->
    io:format("~tp\n", [X]).

-doc "Print `Str` (a string, a binary or an iolist) as text.".
-spec s(unicode:chardata()) -> ok.
s(Str) ->
    io:format("~ts\n", [Str]).

%%%-------------------------------------------------------------------
%%% Distribution
%%%-------------------------------------------------------------------

-doc """
Call `M:F(A1, ..., An)` on this node and on all connected nodes.

The result has one element for each node, in the order of
`[node() | nodes()]`: `{ok, Result}` or `{Class, Reason}`.
""".
-spec mc(module(), atom(), [term()]) -> [{ok, term()} | {atom(), term()}].
mc(M, F, As) ->
    erpc:multicall([node() | nodes()], M, F, As).

%%%-------------------------------------------------------------------
%%% Tracing with dbg
%%%
%%% Based on code by Serge Aleynikov, erlang-questions, Sep 2006.
%%%-------------------------------------------------------------------

-doc "Read trace data from `File` and print it.".
dbgtc(File) ->
    Print = fun({trace, _, call, {M, F, A}}, _) ->
                    io:format("call: ~w:~w~w\n", [M, F, A]);
               ({trace, _, return_from, {M, F, A}, R}, _) ->
                    io:format("retn: ~w:~w/~w -> ~w\n", [M, F, A, R]);
               (Msg, Acc) ->
                    io:format("~w: ~w\n", [Msg, Acc])
            end,
    dbg:trace_client(file, File, {Print, []}).

-doc "Start the dbg tracer and trace all functions in `Module`.".
dbgon(Module) ->
    case dbg:tracer() of
        {ok, _} ->
            dbg:p(all, call),
            dbg:tpl(Module, ?RETURN_TRACE),
            ok;
        Error ->
            Error
    end.

-doc """
Start the dbg tracer for `Module`.

If the second argument is an atom, trace only the function with that
name.  If it is a string, trace all functions and write the trace to
the file with that name (read the file with `dbgtc/1`).
""".
dbgon(Module, Fun) when is_atom(Fun) ->
    {ok, _} = dbg:tracer(),
    dbg:p(all, call),
    dbg:tpl(Module, Fun, ?RETURN_TRACE),
    ok;
dbgon(Module, File) when is_list(File) ->
    {ok, _} = dbg:tracer(port, dbg:trace_port(file, File)),
    dbg:p(all, call),
    dbg:tpl(Module, ?RETURN_TRACE),
    ok.

-doc "Add call trace for all functions in `Module`.".
dbgadd(Module) ->
    dbg:tpl(Module, ?RETURN_TRACE),
    ok.

-doc "Add call trace for the function `Module:Fun`.".
dbgadd(Module, Fun) ->
    dbg:tpl(Module, Fun, ?RETURN_TRACE),
    ok.

-doc "Remove call trace from all functions in `Module`.".
dbgdel(Module) ->
    dbg:ctpl(Module),
    ok.

-doc "Remove call trace from the function `Module:Fun`.".
dbgdel(Module, Fun) ->
    dbg:ctpl(Module, Fun),
    ok.

-doc "Stop the dbg tracer.".
dbgoff() ->
    dbg:stop().

%%%-------------------------------------------------------------------
%%% Help
%%%-------------------------------------------------------------------

-doc "Print the shell help, then the commands of this module.".
-spec help() -> true.
help() ->
    shell_default:help(),
    io:put_chars(
      """
      ** user extended commands **
      mk()          -- run make in the configured dirs, load, unit test
      mk(Dir)       -- run make in Dir, load, unit test
      mk0()         -- run make in the current directory
      mkl()         -- run make in the current directory, load
      l()           -- load all changed modules
      nl()          -- load all changed modules on all connected nodes
      mm()          -- list changed modules
      mm(Node)      -- list changed modules on Node
      u()           -- load all changed modules and run their unit tests
      u(M)          -- run the unit tests of module M
      p(X)          -- print the full value of X
      s(Str)        -- print Str as text
      mc(M,F,Args)  -- call M:F(Args) on this node and all connected nodes
      dbgtc(File)   -- use dbg:trace_client() to read data from File
      dbgon(M)      -- enable dbg tracer on all funs in module M
      dbgon(M,Fun)  -- enable dbg tracer for module M and function Fun
      dbgon(M,File) -- enable dbg tracer for module M and log to File
      dbgadd(M)     -- enable call tracer for module M
      dbgadd(M,F)   -- enable call tracer for function M:F
      dbgdel(M)     -- disable call tracer for module M
      dbgdel(M,F)   -- disable call tracer for function M:F
      dbgoff()      -- disable dbg tracer (calls dbg:stop/0)

      """),
    true.
