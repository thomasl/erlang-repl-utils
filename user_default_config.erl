%%%-------------------------------------------------------------------
%%% File    : user_default_config.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%%
%%% Created : 23 May 2019 by Thomas Lindgren <>
%%%-------------------------------------------------------------------

%% New version of user_default_config
%% - store data as erlang terms in plain config file
%% - read/write API
%%
%% - multiple values per key! (e.g., {ebin, "..."})?
%% - no hierarchical keys yet?
%% - only explicit config files (in this API)
%% - should have env.vars too
%%
%% We use a gen_server to manage this. The size of the config is
%% assumed to be negligible.
%%
%% Current status: MUCH nicer than the old method of using ets + tab2file
%%
%% UNFINISHED
%% - save_config/0,1 uses a 'modified' flag to control whether to save
%%   stuff or not ... not quite satisfactory
%%   * we want to avoid overwriting the old config file by mistake
%%   * overwrite does not preserve comments

-module(user_default_config).
-behaviour(gen_server).
-export(
   [load_config/1,
    save_config/1,
    save_config/0,
    get/1,
    get/2,
    put/2,
    get_all_keys/0]
  ).

-define(SERVER, ?MODULE).
-define(dbg(Str, Xs), io:format(Str, Xs)).
%-define(dbg(Str, Xs), ok).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {config_file, keys=empty(), modified}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The main API. Starts the config server if not started yet.
%%
%% - note that we still need to load the initial config file explicitly;
%%   perhaps this should be implicit too? (handled at gen_server:start_link)
%% - not extremely elegant to do ensure_started() everywhere

load_config(File) ->
    ensure_started(),
    gen_server:call(?SERVER, {load_config, File}).

save_config(File) ->
    ensure_started(),
    gen_server:call(?SERVER, {save_config, File}).

save_config() ->
    ensure_started(),
    gen_server:call(?SERVER, save_config).

get(Key) ->
    ensure_started(),
    gen_server:call(?SERVER, {get, Key}).

get(Key, Default) ->
    ensure_started(),
    case gen_server:call(?SERVER, {get, Key}) of
	{found, Val} ->
	    Val;
	not_found ->
	    Default
    end.

get_all_keys() ->
    ensure_started(),
    gen_server:call(?SERVER, get_all_keys).

put(Key, Val) ->
    ensure_started(),
    gen_server:call(?SERVER, {put, Key, Val}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%

ensure_started() ->
    case start_link() of
	{ok, PID} ->
	    ok;
	{error, {already_started, PID}} ->
	    %% ugh, but that's the API
	    ok;
	Err ->
	    %% printout/log?
	    Err
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

init(_Ignored) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({load_config, File}, From, State) ->
    case State#state.config_file of
	undefined ->
	    case server_load_config(File) of
		{ok, Conf} ->
		    {reply, ok, State#state{config_file=File, keys=Conf}};
		{error, Rsn}=Err ->
		    {reply, Err, State}
	    end;
	ConfigFile ->
	    Reply = {ok, {already_loaded, ConfigFile}},
	    {reply, Reply, State}
    end;
handle_call({save_config, File}, From, State) ->
    case State#state.config_file of
	undefined ->
	    Reply = server_save_config(State#state.keys, File),
	    {reply, Reply, State#state{modified=false}};
	ConfigFile ->
	    case State#state.modified of
		true ->
		    Reply = server_save_config(State#state.keys, File),
		    {reply, Reply, State#state{modified=false}};
		_ ->
		    {reply, {ok, unchanged}, State}
	    end
    end;
handle_call(save_config, From, State) ->
    case State#state.config_file of
	undefined ->
	    Reply = {error, no_config_loaded},
	    {reply, Reply, State};
	ConfigFile ->
	    case State#state.modified of
		false ->
		    {reply, {ok, unchanged}, State};
		true ->
		    Reply = server_save_config(State#state.keys, ConfigFile),
		    {reply, Reply, State#state{modified=false}}
	    end
    end;
handle_call({get, Key}, From, State) ->
    Reply = get_key(Key, State),
    {reply, Reply, State};
handle_call({put, Key, Value}, From, State) ->
    {Reply, NewState} = put_key(Key, Value, State),
    {reply, Reply, NewState#state{modified=true}};
handle_call(get_all_keys, _From, State) ->
    Reply = get_all_keys(State),
    {reply, Reply, State};
handle_call(Other, _From, State) ->
    {reply, {error, {unknown_call, Other}}, State}.

%%

server_load_config(File) ->
    Cfg_terms =
	case file:consult(File) of
	    {ok, Terms} ->
		Terms;
	    Err ->
		io:format("Load config ~s -> ~p, config cleared/empty\n",
			  [File, Err]),
		[]
	end,
    make_config(Cfg_terms).

%% NB: file is saved on host where gen_server is running. Probably
%% not a killer issue. Right ...?
%%
%% UNFINISHED
%% - many io:format/3 seems costly, but we assume the config is small
%%   and that this op is infrequent

server_save_config(Conf, File) ->
    KVs = lists:sort(dict:to_list(Conf)),
    case file:open(File, [write]) of
	{ok, FD} ->
	    io:format(FD, "%% -*- Erlang -*-\n\n", []),
	    lists:foreach(
	      fun(KV) ->
		      io:format(FD, "~p.\n", [KV])
	      end,
	      KVs),
	    ok = file:close(FD),
	    ok;
	Err ->
	    io:format("Error when saving to ~s -> ~p\n",
		      [File, Err]),
	    Err
    end.

get_key(Key, State) ->
    case dict:find(Key, State#state.keys) of
	{ok, Val} ->
	    {found, Val};
	_ ->
	    not_found
    end.

get_all_keys(State) ->
    {ok, lists:sort(dict:to_list(State#state.keys))}.

put_key(Key, Val, State) ->
    NewDict = dict:store(Key, Val, State#state.keys),
    {ok, State#state{keys = NewDict}}.

%% Return {ok, Conf} | {error, Rsn}
%%
%% UNFINISHED
%% - better error handling
%% - this should be a bit more flexible, update later (foldl?)

make_config(Terms) ->
    {ok, dict:from_list(Terms)}.

%%

empty() ->
    dict:new().

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
