%%% File    : user_default_config.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created :  8 Nov 2008 by Thomas Lindgren <>
%%
%% Persistent user configuration.
%%
%% NOTES
%% - hmm, this is getting pretty perl-y (massively configurable :-)
%% - adding the "scripted keys" could replace the .erlang init file
%%   with something perhaps more flexible and useful
%%
%% EXTENSIONS
%% - can we make this visible to erlc somehow?
%%   * erlc is just a wrapper around compile
%% - scripted keys, e.g., "$HOME" expanding to current home dir
%%   * always? on loading the config?
%%   * or actually run scripts
%% - startup tasks
%%   * save tasks to be run (e.g., set code paths)
%%   * run them on loading config, or on other conditions
%%   * useful to e.g., locate smart_exceptions.beam and make it visible
%% - optionally logging when changes occur
%%   * custom log messages
%% - transforming derived values before entering them into table
%% - declare and list namespaces in a structured way
%%   * define default namespaces
%%   * declare namespace
%%   * list namespace
%%   * erase namespace? hide?
%% - multiple config files? multiple config tables? (useful w/ multiple ns?)
%%   * save config versions
%%   * revert to old config
%% - operating on multiple keys w/o saving many times
%%   (transactionally?)

-module(user_default_config).
-export([get_key/3,
	 put_key/2,
	 erase_key/1,
	 delete_key/1,
	 save_config/0,
	 erase_config/0,
	 ensure_started/0,
	 start/0,
	 stop/0,
	 show_config/0,
	 list_config/0,
	 config_file/0
	]).
-compile(export_all).

-define(config, user_default_config).
-define(config_proc, user_default_config_owner).
-define(home_defaults, 
	strip_ws(os:cmd("echo $HOME/defaults/user_defaults_config.tab"))).
-define(config_startup_timeout, 2000).

%% default namespace [should also have a meta-ns for internal keys?]
-define(ns, user).

config_file() ->
    ?home_defaults.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_key(Key, Cmd_line, Default) ->
    get_key(?ns, Key, Cmd_line, Default).

get_key(NS, Key, Cmd_line, Default) ->
    ensure_started(),
    case ets:lookup(?config, {NS, Key}) of
	[{_, Val}] ->
	    Val;
	[] ->
	    Val = derive_value(Cmd_line, Default),
	    put_key({NS, Key}, Val),
	    Val
    end.

put_key(Key, Val) ->
    put_key(?ns, Key, Val).

put_key(NS, Key, Val) ->
    ensure_started(),
    ets:insert(?config, {{NS, Key}, Val}),
    save_config(),
    ok.

erase_key(NS, Key) ->
    delete_key(NS, Key).

erase_key(Key) ->
    delete_key(Key).

delete_key(Key) ->
    delete_key(?ns, Key).

delete_key(NS, Key) ->
    ensure_started(),
    ets:delete(?config, {NS, Key}),
    save_config(),
    ok.

%% derive_value tries to figure out a desired default if none is stored
%% in the current table.
%%
%% This is done by looking at the command line or using a default value
%% if none can be found.
%%
%% EXTENSIONS
%% - default could be a fun to be applied
%% - we could pass a mode being a fun to apply to the value(s)

derive_value(Cmd_line, Default) ->
    derive_value(Cmd_line, Default, first).

derive_value(Cmd_line_str, Default, Mode) ->
    Cmd_line = list_to_atom(Cmd_line_str),
    case init:get_argument(Cmd_line) of
	{ok, [[Val]]} ->
	    Val;
	{ok, Vals_lst} ->
	    case Mode of
		all ->
		    combine_vals(Mode, 
				 [ Val || Vals <- Vals_lst,
					  Val <- Vals ], 
				 Default);
		{all, _Sep} ->
		    combine_vals(Mode, 
				 [ Val || Vals <- Vals_lst,
					  Val <- Vals ], 
				 Default);
		_ ->
		    io:format("Command line key defined multiple times. "
			      "Using last definition.\n", []),
		    Vals = lists:last(Vals_lst),
		    case Vals of
			[Val] ->
			    Val;
			_ ->
			    combine_vals(Mode, Vals, Default)
		    end
	    end;
	_ ->
	    Default
    end.

combine_vals(Mode, Vals, Default) ->
    case Mode of
	all ->
	    lists:flatten(splat(" ", Vals));
	{all, Sep} ->
	    lists:flatten(splat(Sep, Vals));
	first ->
	    hd(Vals);
	last ->
	    lists:last(Vals);
	default ->
	    Default
    end.

%%%%%%%%%%

splat(Sep, [Val|Vals]) ->
    [Val|splat_rest(Sep, Vals)];
splat(_Sep, []) ->
    [].

splat_rest(Sep, [Val|Vals]) ->
    [Sep, Val|splat_rest(Sep, Vals)];
splat_rest(_Sep, []) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ensure that the config process has started

ensure_started() ->
    case whereis(?config_proc) of
	undefined ->
	    start();
	_ ->
	    ok
    end.

%% Start the config table owner. If there is a saved table in a well-known
%% location, we read that table. If unable to read the table, just start a
%% new one.
%%
%% The config table is saved periodically and at every structured update.
%%
%% Startup is synchronous: the semi-parent waits until process exists, or
%% fails.
%%
%% this module nearly never changes, so we use a fun
%% - otherwise, we'd run the risk of killing the process when
%%   the fun object code ages out
%%
%% - avoid proc_lib to work without otp?

start() ->
    Ref = erlang:make_ref(),
    Parent = self(),
    proc_lib:spawn(
      fun() ->
	      Tab_file = ?home_defaults,
	      case ets:file2tab(Tab_file) of
		  {ok, ?config} ->
		      %% great! we're done
		      ok;
		  {ok, OtherTab} ->
		      io:format("File ~p should have contained a table "
				"named ~p, but held ~p.\n",
				[Tab_file, ?config, OtherTab]),
		      case catch ets:new(?config, [named_table, public, set]) of
			  {'EXIT', Rsn} ->
			      io:format("Unable to create ~p: ~p\n",
					[?config, Rsn]);
			  _OK ->
			      ok
		      end;
		  Err ->
		      io:format("Unable to read file ~p: ~p."
				"Using empty config.\n",
				[Tab_file, Err]),
		      case catch ets:new(?config, [named_table, public, set]) of
			  {'EXIT', Rsn} ->
			      io:format("Unable to create ~p: ~p\n",
					[?config, Rsn]);
			  _OK ->
			      ok
		      end
	      end,
	      register(?config_proc, self()),
	      Parent ! {started, Ref},   %% tell parent to continue
	      loop(?config, Tab_file)
      end),
    receive
	{started, Ref} ->
	    ok
    after ?config_startup_timeout ->
	    exit({unable_to_start, ?config_proc})
    end.

%% Periodically save the table to the file. If notified that the
%% process should stop, save the table. Otherwise just stop.

loop(Table, Tab_file) ->
    receive
	save ->
	    ets:tab2file(Table, Tab_file),
	    loop(Table, Tab_file);
	stop ->
	    ets:tab2file(Table, Tab_file);
	erase ->
	    ets:delete_all_objects(Table),
	    loop(Table, Tab_file);
	{started, Asker} ->
	    Asker ! ok,
	    loop(Table, Tab_file)
    after get_timeout() ->
	    ets:tab2file(Table, Tab_file),
	    loop(Table, Tab_file)
    end.

erase_config() ->
    ?config_proc ! erase.

save_config() ->
    ?config_proc ! save.

stop() ->
    ?config_proc ! stop.

get_timeout() ->
    60000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_config() ->
    ets:tab2list(?config).

show_config() ->
    lists:foreach(
      fun({{?ns, Key}, Val}) ->
	      io:format(":~p -> ~p\n", [Key, Val]);
	 ({{NS, Key}, Val}) ->
	      io:format("~p:~p -> ~p\n", [NS, Key, Val])
      end,
      list_config()
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Strip trailing whitespace from a line.
%%
%% An explicitly escaped char never counts as whitespace.

strip_ws([$\\, C|Cs]) ->
    [$\\, C|strip_ws(Cs)];
strip_ws([C|Cs]) ->
    case strip_ws(Cs) of
	[] ->
	    if
		C == $\s ;
		C == $\n ;
		C == $\t ; 
		C == $\r ->
		    [];
		true ->
		    [C]
	    end;
	NewCs ->
	    [C|NewCs]
    end;
strip_ws([]) ->
    [].

