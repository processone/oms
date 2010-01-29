%%%-------------------------------------------------------------------
%%% File    : oms_listener.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created :  4 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_listener).

-behaviour(gen_server).

%% API
-export([start_link/0, start_listener/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("p1_logger.hrl").

-define(RTMP_PORT, 1935).
-define(TCP_SEND_TIMEOUT, 32000).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    [Opts|_] = oms_config:get_opts(listen),
    Addr = case inet_parse:address(
		  oms_config:get_opt(address, Opts)) of
	       {ok, IP} ->
		   IP;
	       _Err ->
		   {0, 0, 0, 0}
	   end,
    Port = case catch list_to_integer(
			oms_config:get_opt(port, Opts)) of
	       Int when is_integer(Int), Int > 0 ->
		   Int;
	       _ ->
		   ?RTMP_PORT
	   end,
    {_Pid, _MonitorRef} =
	spawn_monitor(?MODULE, start_listener, [Addr, Port]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, badarg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    ?ERROR_MSG("listener failed", []),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_listener(Addr, Port) ->
    case gen_tcp:listen(Port, [binary,
			       {ip, Addr},
			       {packet, 0},
			       {active, false},
			       {reuseaddr, true},
			       {nodelay, true},
			       {send_timeout, ?TCP_SEND_TIMEOUT},
			       {keepalive, true}]) of
	{ok, ListenSocket} ->
	    accept(ListenSocket);
	Err ->
	    Err
    end.

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    case {inet:peername(Socket),
		  inet:sockname(Socket)} of
		{{ok, PeerAddr}, {ok, Addr}} ->
		    ?INFO_MSG("accepted connection: ~w -> ~w",
			      [PeerAddr, Addr]),
		    case rtmp_socket:open(Socket, PeerAddr) of
			{ok, Pid} ->
			    gen_tcp:controlling_process(Socket, Pid);
			Err ->
			    Err
		    end;
		Err ->
		    ?ERROR_MSG("unable to fetch peername: ~p", [Err]),
		    Err
	    end,
	    accept(ListenSocket);
	Err ->
	    Err
    end.
