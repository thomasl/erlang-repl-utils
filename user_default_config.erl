-module(user_default_config).
-moduledoc """
A small key-value configuration store for the Erlang shell.

The configuration is read from a plain file of Erlang terms, one
`{Key, Value}.` tuple for each key (see `.econf` for an example).  If
a key occurs more than once, the last value is used.

A `gen_server` keeps the configuration.  Each API function starts the
server if it does not run.  The server is not linked to the caller, so
the configuration stays when the shell process restarts after an error.

`save_config/0,1` writes the terms in sorted order.  Comments in the
original file are not kept.
""".
-behaviour(gen_server).

%% API
-export([load_config/1,
         save_config/0,
         save_config/1,
         get/1,
         get/2,
         put/2,
         get_all_keys/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%% get/1 and put/2 are also BIFs.  This module does not use the BIFs.
-compile({no_auto_import, [get/1, put/2]}).

-define(SERVER, ?MODULE).

-record(state,
        {file :: file:name_all() | undefined,  % the loaded config file
         conf = #{} :: #{term() => term()},
         modified = false :: boolean()}).      % changed after load or save

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-doc """
Load the configuration file `File`.

If the file cannot be read, a message is printed and the configuration
is empty.  Only the first call loads a file.  Subsequent calls return
`{ok, {already_loaded, LoadedFile}}`.
""".
-spec load_config(file:name_all()) ->
          ok | {ok, {already_loaded, file:name_all()}}.
load_config(File) ->
    call({load_config, File}).

-doc """
Write the configuration to the loaded config file, if a value changed
after the load or the last save.
""".
-spec save_config() -> ok | {ok, unchanged} | {error, term()}.
save_config() ->
    call(save_config).

-doc "Write the configuration to `File`.".
-spec save_config(file:name_all()) -> ok | {error, term()}.
save_config(File) ->
    call({save_config, File}).

-doc "Get the value of `Key`.".
-spec get(term()) -> {found, term()} | not_found.
get(Key) ->
    call({get, Key}).

-doc "Get the value of `Key`, or `Default` if the key is not set.".
-spec get(term(), Default) -> term() | Default.
get(Key, Default) ->
    case get(Key) of
        {found, Val} -> Val;
        not_found -> Default
    end.

-doc "Set the value of `Key`.".
-spec put(term(), term()) -> ok.
put(Key, Val) ->
    call({put, Key, Val}).

-doc "Get all keys and their values, sorted.".
-spec get_all_keys() -> {ok, [{term(), term()}]}.
get_all_keys() ->
    call(get_all_keys).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({load_config, File}, _From, #state{file = undefined} = State) ->
    {reply, ok, State#state{file = File, conf = read_config(File)}};
handle_call({load_config, _File}, _From, State) ->
    {reply, {ok, {already_loaded, State#state.file}}, State};
handle_call(save_config, _From, #state{file = undefined} = State) ->
    {reply, {error, no_config_loaded}, State};
handle_call(save_config, _From, #state{modified = false} = State) ->
    {reply, {ok, unchanged}, State};
handle_call(save_config, _From, State) ->
    save(State#state.file, State);
handle_call({save_config, File}, _From, State) ->
    save(File, State);
handle_call({get, Key}, _From, State) ->
    Reply = case State#state.conf of
                #{Key := Val} -> {found, Val};
                #{} -> not_found
            end,
    {reply, Reply, State};
handle_call({put, Key, Val}, _From, #state{conf = Conf} = State) ->
    {reply, ok, State#state{conf = Conf#{Key => Val}, modified = true}};
handle_call(get_all_keys, _From, State) ->
    {reply, {ok, lists:sort(maps:to_list(State#state.conf))}, State};
handle_call(Other, _From, State) ->
    {reply, {error, {unknown_call, Other}}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

call(Request) ->
    case gen_server:start({local, ?SERVER}, ?MODULE, [], []) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,
    gen_server:call(?SERVER, Request).

read_config(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            maps:from_list(lists:filter(fun is_key_value/1, Terms));
        {error, _} = Error ->
            io:format("Load config ~ts -> ~tp, config is empty\n",
                      [File, Error]),
            #{}
    end.

is_key_value({_Key, _Val}) ->
    true;
is_key_value(Term) ->
    io:format("Config term ignored (not {Key, Value}): ~tp\n", [Term]),
    false.

%% The file is written on the host where the server runs.
save(File, #state{conf = Conf} = State) ->
    Text = ["%% -*- Erlang -*-\n\n",
            [io_lib:format("~tp.\n", [KV])
             || KV <- lists:sort(maps:to_list(Conf))]],
    case file:write_file(File, unicode:characters_to_binary(Text)) of
        ok ->
            %% The loaded config file is up to date only if it was the
            %% file that was written.
            Modified = State#state.modified andalso File =/= State#state.file,
            {reply, ok, State#state{modified = Modified}};
        {error, _} = Error ->
            io:format("Error when saving to ~ts -> ~tp\n", [File, Error]),
            {reply, Error, State}
    end.
