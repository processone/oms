%%%-------------------------------------------------------------------
%%% File    : oms_sup.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created :  4 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    RTMPSockSup =
	{oms_rtmp_socket_sup,
	 {oms_tmp_sup, start_link,
	  [oms_rtmp_socket_sup, rtmp_socket]},
	 permanent,
	 infinity,
	 supervisor,
	 [oms_tmp_sup]},
    RTMPSessionSup =
	{oms_rtmp_session_sup,
	 {oms_tmp_sup, start_link,
	  [oms_rtmp_session_sup, rtmp_session]},
	 permanent,
	 infinity,
	 supervisor,
	 [oms_tmp_sup]},
    Listener =
	{oms_listener,
	 {oms_listener, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [oms_listener]},
    AppMgr =
	{oms_app_mgr,
	 {oms_app_mgr, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [oms_app_mgr]},
    FileStreamSup =
	{oms_file_stream_sup,
	 {oms_tmp_sup, start_link,
	  [oms_file_stream_sup, oms_file_stream]},
	 permanent,
	 infinity,
	 supervisor,
	 [oms_tmp_sup]},
    StreamSup =
	{oms_stream_sup,
	 {oms_tmp_sup, start_link,
	  [oms_stream_sup, oms_stream]},
	 permanent,
	 infinity,
	 supervisor,
	 [oms_tmp_sup]},
    SM =
	{oms_sm,
	 {oms_sm, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [oms_sm]},
    SOMgr =
	{oms_so_mgr,
	 {oms_so_mgr, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [oms_so_mgr]},
    Config =
	{oms_config,
	 {oms_config, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [oms_config]},
    {ok, {{one_for_one, 10, 1},
	  [Config,
	   AppMgr,
	   RTMPSockSup,
	   RTMPSessionSup,
	   FileStreamSup,
	   StreamSup,
	   SM,
	   SOMgr,
	   Listener]}}.

%%====================================================================
%% Internal functions
%%====================================================================
