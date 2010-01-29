%%%-------------------------------------------------------------------
%%% File    : oms_app_mgr.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created :  4 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_app_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0, load/1, get_application/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("p1_logger.hrl").

-record(oms_app, {name, connection_mod, stream_mod, options}).

-record(state, {app_conf_dir, app_beam_dir}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load(Path) ->
    gen_server:cast(?MODULE, {load, Path}).

get_application(AppName) ->
    case ets:lookup(oms_app, AppName) of
	[#oms_app{connection_mod = ConnectionMod,
		  stream_mod = StreamMod,
		  options = Options}] ->
	    {ok, ConnectionMod, StreamMod, Options};
	_ ->
	    {error, enoent}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    [Opts|_] = oms_config:get_opts(main),
    AppConfDir = filename:absname(
		   oms_config:get_opt(app_conf_dir, Opts, "app_configs")),
    AppBeamDir = filename:absname(
		   oms_config:get_opt(app_beam_dir, Opts, "")),
    ConfFiles = filelib:wildcard(filename:join(AppConfDir, "*.cfg")),
    lists:foreach(fun(Path) -> load(Path) end, ConfFiles),
    catch ets:new(oms_app, [named_table, public,
			    {keypos, #oms_app.name}]),
    {ok, #state{app_conf_dir = AppConfDir,
		app_beam_dir = AppBeamDir}}.

handle_call(_Request, _From, State) ->
    {reply, badarg, State}.

handle_cast({load, Path}, State) ->
    case catch do_load(State, Path) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("Loading application from ~p failed:~n\t~p",
		       [Path, Reason]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_load(#state{app_conf_dir = AppConfDir,
	       app_beam_dir = AppBeamDir},
	Path) ->
    ConfigPath = filename:join(AppConfDir, Path),
    case oms_config:parse(ConfigPath) of
	{ok, Config} ->
	    case process_config(ConfigPath, AppBeamDir, Config) of
		ok ->
		    ok;
		{error, Reason} = Err ->
		    ?ERROR_MSG("failed to load application from ~p: ~s",
			       [ConfigPath, format_error(Reason)]),
		    Err
	    end;
	{error, Reason} = Err ->
	    ?ERROR_MSG("failed to load application from ~p: ~s",
		       [ConfigPath, format_error(Reason)]),
	    Err
    end.

process_config(ConfigPath, AppBeamDir, Config) ->
    [MainOpts|_] = oms_config:get_opts(main, Config),
    case oms_config:get_opt(name, MainOpts, "") of
	"" ->
	    ?ERROR_MSG("application name is undefined "
		       "in ~p", [ConfigPath]),
	    {error, bad_config};
	Name ->
	    ConnectionModName =	oms_config:get_opt(
				  netconnection_module,
				  MainOpts, ""),
	    StreamModName = oms_config:get_opt(
			      netstream_module, MainOpts, ""),
	    BeamDir = oms_config:get_opt(beam_dir, MainOpts, AppBeamDir),
	    [Options|_] = oms_config:get_opts(options, Config),
	    case {mod_load(BeamDir, ConnectionModName, Options),
		  mod_load(BeamDir, StreamModName, Options)} of
		{{ok, ConnectionMod}, {ok, StreamMod}} ->
		    ets:insert(
		      oms_app,
		      #oms_app{name = Name,
			       connection_mod = ConnectionMod,
			       stream_mod = StreamMod,
			       options = Options}),
		    ?INFO_MSG("loaded application: ~p", [Name]);
		_Err ->
		    {error, load_failed}
	    end
    end.

mod_load(_Dir, "", _Options) ->
    {ok, undefined};
mod_load(Dir, Name, Options) ->
    {LoadFun, Path} = case Dir of
			  "" ->
			      {load_file, list_to_atom(Name)};
			  _ ->
			      {load_abs, filename:join(Dir, Name)}
		      end,
    case code:LoadFun(Path) of
	{module, Module} ->
	    case erlang:function_exported(Module, load, 1) of
	  	true ->
		    Module:load(Options);
		false ->
		    ok
	    end,
	    {ok, Module};
	{error, What} = Err ->
	    Reason = format_error(What),
	    ?ERROR_MSG("failed to load module \"~s\": ~s", [Path, Reason]),
	    Err
    end.

format_error(load_failed) ->
    "unable to load modules";
format_error(bad_config) ->
    "bad configuration";
format_error(nofile) ->
    format_error(enoent);
format_error(Err) ->
    case file:format_error(Err) of
	"unknown POSIX error" ->
	    Err;
	Reason ->
	    Reason
    end.
