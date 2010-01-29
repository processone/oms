%%%-------------------------------------------------------------------
%%% File    : rtmp.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 27 Nov 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(rtmp_socket).

-define(GEN_FSM, gen_fsm).

-behaviour(?GEN_FSM).

%% API
%%-compile(export_all).

-export([start_link/2,
	 setopts/2,
	 getopts/2,
	 connect/2,
	 connect/3,
	 open/2,
	 send/2,
	 sync_send/2,
	 alloc_csid/1,
	 close/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

%% gen_fsm states
-export([wait_for_first_handshake/2,
	 wait_for_second_handshake/2,
	 connecting/3,
	 wait_for_first_handshake_reply/2,
	 wait_for_second_handshake_reply/2,
	 session_established/2,
	 session_established/3]).

-include("p1_logger.hrl").
-include("rtmp.hrl").

-define(DBG_AV, true).
-define(V3, 3).
-define(DICT, dict).
-define(HANDSHAKE_TIMEOUT, 10000).
-define(SYNC_SEND_TIMEOUT, 60000).

-record(state, {sock,
		type,
		server,
		port,
		caller,
		tref,
		addr,
		tcp_buf = <<>>,
		bytes_in = {0, 0},
		peer_timestamp,
		window_size = ?DEFAULT_WINDOW_SIZE,
		peer_window_size = ?DEFAULT_WINDOW_SIZE,
		rtmp_codec,
		session,
		owner
	       }).

%%====================================================================
%% API
%%====================================================================
start_link(Socket, Addr) ->
    ?GEN_FSM:start_link(?MODULE, [Socket, Addr], []).

connect(Server, Port) ->
    connect(Server, Port, self()).

connect(Server, Port, Owner) ->
    case ?GEN_FSM:start(?MODULE, [Server, Port, Owner], []) of
	{ok, Pid} ->
	    case catch ?GEN_FSM:sync_send_event(Pid, connect) of
		{'EXIT', _} ->
		    {error, timeout};
		ok ->
		    {ok, Pid};
		Res ->
		    Res
	    end;
	Err ->
	    Err
    end.

open(Socket, Addr) ->
    supervisor:start_child(oms_rtmp_socket_sup, [Socket, Addr]).

close(Pid) ->
    ?GEN_FSM:send_all_state_event(Pid, close).

send(Pid, RTMP) when is_record(RTMP, rtmp_msg) ->
    ?GEN_FSM:send_event(Pid, RTMP).

sync_send(Pid, RTMP) when is_record(RTMP, rtmp_msg) ->
    case catch ?GEN_FSM:sync_send_event(
		  Pid, RTMP, ?SYNC_SEND_TIMEOUT) of
	{'EXIT', _} ->
	    {error, einval};
	Res ->
	    Res
    end.

setopts(Pid, Opts) ->
    ?GEN_FSM:send_event(Pid, {setopts, Opts}).

getopts(Pid, Opts) ->
    case catch ?GEN_FSM:sync_send_all_state_event(
		  Pid, {getopts, Opts}, ?SYNC_SEND_TIMEOUT) of
	{'EXIT', _} ->
	    {error, einval};
	Res ->
	    Res
    end.

alloc_csid(Pid) ->
    case catch ?GEN_FSM:sync_send_all_state_event(
		  Pid, alloc_csid, ?SYNC_SEND_TIMEOUT) of
	{'EXIT', _} ->
	    {error, einval};
	Res ->
	    Res
    end.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Socket, Addr]) ->
    activate(Socket),
    ?INFO_MSG("connection opened for ~p", [Addr]),
    TRef = ?GEN_FSM:send_event_after(?HANDSHAKE_TIMEOUT, stop),
    RTMPCodec = rtmp_codec:new(),
    {ok, wait_for_first_handshake,
     #state{sock = Socket, tref = TRef,
	    rtmp_codec = RTMPCodec,
	    addr = Addr, type = server}};
init([Server, Port, Owner]) ->
    RTMPCodec = rtmp_codec:new(),
    erlang:monitor(process, Owner),
    {ok, connecting,
     #state{server = Server, port = Port,
	    rtmp_codec = RTMPCodec,
	    owner = Owner, type = client}}.

wait_for_first_handshake(Data, State) when is_binary(Data) ->
    case <<(State#state.tcp_buf)/binary, Data/binary>> of
	<<?V3, TimeStamp:32, _MBZ:32,
	 RndData:1528/binary, Tail/binary>> ->
	    tcp_send(State#state.sock,
		     <<?V3, 0:32, 0:32, RndData/binary>>),
	    ?GEN_FSM:send_event(self(), Tail),
	    {next_state, wait_for_second_handshake,
	     State#state{peer_timestamp = TimeStamp, tcp_buf = <<>>}};
	<<?V3, _/binary>> = NewData ->
	    {next_state, wait_for_first_handshake,
	     State#state{tcp_buf = NewData}};
	_ ->
	    {stop, normal, State}
    end;
wait_for_first_handshake(_Event, State) ->
    {stop, normal, State}.

wait_for_second_handshake(Data, State) when is_binary(Data) ->
    case <<(State#state.tcp_buf)/binary, Data/binary>> of
	<<_Time:32, _Delta:32, RndData:1528/binary, Tail/binary>> ->
	    tcp_send(State#state.sock,
		     <<(State#state.peer_timestamp):32,
		      0:32, RndData/binary>>),
	    ?GEN_FSM:cancel_timer(State#state.tref),
	    case rtmp_session:start(self(), State#state.addr) of
		{ok, Session} ->
		    ?GEN_FSM:send_event(self(), Tail),
		    {next_state, session_established,
		     State#state{tcp_buf = <<>>, session = Session}};
		_Err ->
		    {stop, normal, State}
	    end;
	NewData ->
	    {next_state, wait_for_second_handshake,
	     State#state{tcp_buf = NewData}}
    end;
wait_for_second_handshake(_Event, State) ->
    {stop, normal, State}.

connecting(connect, Caller, State) ->
    case gen_tcp:connect(State#state.server,
			 State#state.port,
			 [{active, false}, binary]) of
	{ok, Sock} ->
	    activate(Sock),
	    Handshake = <<0:32, 0:32, (handshake())/binary>>,
	    tcp_send(Sock, <<3, Handshake/binary>>),
	    {next_state, wait_for_first_handshake_reply,
	     State#state{sock = Sock, caller = Caller}};
	Err ->
	    {stop, normal, Err, State}
    end;
connecting(_Event, _From, State) ->
    {stop, normal, State}.

wait_for_first_handshake_reply(Data, State) when is_binary(Data) ->
    case <<(State#state.tcp_buf)/binary, Data/binary>> of
	<<?V3, TimeStamp:32, _Delta:32,
	 RndData:1528/binary, Tail/binary>> ->
	    tcp_send(State#state.sock,
		     <<TimeStamp:32, 0:32, RndData/binary>>),
	    ?GEN_FSM:send_event(self(), Tail),
	    {next_state, wait_for_second_handshake_reply,
	     State#state{tcp_buf = <<>>}};
	<<?V3, _/binary>> = NewData ->
	    {next_state, wait_for_first_handshake_reply,
	     State#state{tcp_buf = NewData}};
	_ ->
	    {stop, normal, State}
    end;
wait_for_first_handshake_reply(_Event, State) ->
    {stop, normal, State}.

wait_for_second_handshake_reply(Data, State) when is_binary(Data) ->
    case <<(State#state.tcp_buf)/binary, Data/binary>> of
	<<_Time:32, _Delta:32, _RndData:1528/binary, _Tail/binary>> ->
	    ?GEN_FSM:reply(State#state.caller, ok),
	    {next_state, session_established, State#state{tcp_buf = <<>>}};
	NewData ->
	    {next_state, wait_for_second_handshake_reply,
	     State#state{tcp_buf = NewData}}
    end;
wait_for_second_handshake_reply(_Event, State) ->
    {stop, normal, State}.

session_established(Data, State) when is_binary(Data) ->
    NewData = <<(State#state.tcp_buf)/binary, Data/binary>>,
    {Total, Fraction} = State#state.bytes_in,
    PeerWindowSize = State#state.peer_window_size,
    Size = size(NewData),
    NewTotal = Total + Size,
    NewFraction = Fraction + Size,
    NewState = if NewFraction < PeerWindowSize ->
		       State#state{bytes_in = {NewTotal, NewFraction}};
		  true ->
		       Rem = NewFraction rem PeerWindowSize,
		       send_rtmp(State#state{bytes_in = {NewTotal, Rem}},
				 #rtmp_msg{type = ?ACKNO,
					   stream = 0,
					   chunk_stream = 2,
					   timestamp = 0,
					   data = NewTotal})
	       end,
    case process_data(NewData, NewState) of
	{ok, NewState1} ->
	    {next_state, session_established, NewState1};
	_ ->
	    {stop, normal, State}
    end;
session_established(RTMP, State) when is_record(RTMP, rtmp_msg) ->
    NewState = send_rtmp(State, RTMP),
    {next_state, session_established, NewState};
session_established({setopts, Opts}, State) ->
    NewState = lists:foldl(
		 fun({peer_window_size, Size}, Acc) ->
			 set_peer_bandwidth(Acc, Size, 0);
		    ({window_size, Size}, Acc) ->
			 set_window_size(Acc, Size);
		    ({chunk_size, Size}, Acc) ->
			 set_chunk_size(Acc, Size);
		    (_, Acc) ->
			 Acc
		 end, State, Opts),
    {next_state, session_established, NewState};
session_established(Event, State) ->
    ?INFO_MSG("unexpected event in session_established:~n\t~p", [Event]),
    {next_state, session_established, State}.

session_established(RTMP, _From, State) when is_record(RTMP, rtmp_msg) ->
    NewState = send_rtmp(State, RTMP),
    {reply, ok, session_established, NewState}.

handle_event(close, _StateName, State) ->
    {stop, normal, State#state{owner = undefined}};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event({getopts, OptNames}, _From, StateName, State) ->
    RTMPCodec = State#state.rtmp_codec,
    Res = lists:flatmap(
	    fun(window_size) ->
		    [{window_size, State#state.window_size}];
	       (peer_window_size) ->
		    [{peer_window_size, State#state.peer_window_size}];
	       (chunk_size) ->
		    [{chunk_size,
		      rtmp_codec:get_chunk_size(out, RTMPCodec)}];
	       (peer_chunk_size) ->
		    [{peer_chunk_size,
		      rtmp_codec:get_chunk_size(in, RTMPCodec)}];
	       (_) ->
		    []
	    end, OptNames),
    {reply, {ok, lists:ukeysort(1, Res)}, StateName, State};
handle_sync_event(alloc_csid, _From, StateName, State) ->
    CSID = rtmp_codec:alloc_csid(State#state.rtmp_codec),
    {reply, {ok, CSID}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

handle_info({tcp, Sock, Data}, StateName, #state{sock = Sock} = State) ->
    activate(Sock),
    ?GEN_FSM:send_event(self(), Data),
    {next_state, StateName, State};
handle_info({tcp_closed, Sock}, _StateName, #state{sock = Sock} = State) ->
    ?INFO_MSG("connection reset by peer", []),
    {stop, normal, State};
handle_info({tcp_error, Reason, Sock}, _StateName,
	    #state{sock = Sock} = State) ->
    ?INFO_MSG("connection error: ~s", [inet:format_error(Reason)]),
    {stop, normal, State};
handle_info({'DOWN', _Ref, process, Owner, _}, _StateName,
	    #state{owner = Owner} = State) ->
    {stop, normal, State#state{owner = undefined}};
handle_info(Info, StateName, State) ->
    ?INFO_MSG("unexpected info in ~p:~n\t~p", [StateName, Info]),
    {next_state, StateName, State}.

terminate(_Reason, StateName,
	  #state{session = Session, addr = Addr,
		 rtmp_codec = Codec, owner = Owner}) ->
    if is_pid(Session) ->
	    catch rtmp_session:stop(Session),
	    ?INFO_MSG("connection closed for ~w", [Addr]);
       is_pid(Owner), StateName == session_established ->
	    Owner ! {rtmp_closed, self()};
       true ->
	    ok
    end,
    catch rtmp_codec:close(Codec),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
process_data(Data, #state{rtmp_codec = RTMPCodec} = State) ->
    case catch rtmp_codec:decode(Data, RTMPCodec) of
	{ok, RTMP, NewRTMPCodec, Tail} ->
	    Type = RTMP#rtmp_msg.type,
	    if (Type /= ?VIDEO andalso Type /= ?AUDIO) orelse ?DBG_AV ->
		    ?DEBUG("got RTMP:~n~p", [RTMP]);
	       true ->
		    ok
	    end,
	    NewState = process_rtmp(
			 RTMP, State#state{rtmp_codec = NewRTMPCodec}),
	    process_data(Tail, NewState);
	{more, NewRTMPCodec, Tail} ->
	    process_data(Tail, State#state{rtmp_codec = NewRTMPCodec});
	more ->
	    {ok, State#state{tcp_buf = Data}};
	error ->
	    ?ERROR_MSG("unable to decode chunk:~nData: ~p~nCodec: ~p",
		       [Data, RTMPCodec]),
	    stop
    end.

process_rtmp(#rtmp_msg{stream = 0, chunk_stream = 2,
		       type = Type, data = Data} = Msg, State)
  when Type /= ?USER_CONTROL ->
    Codec = State#state.rtmp_codec,
    case Type of
	?CHKSIZE ->
	    NewCodec = rtmp_codec:set_chunk_size(in, Data, Codec),
	    State#state{rtmp_codec = NewCodec};
	?ABORT ->
	    NewCodec = rtmp_codec:abort(Data, Codec),
	    State#state{rtmp_codec = NewCodec};
	?ACKSIZE ->
	    State#state{peer_window_size = Data};
	?PEERBW when State#state.type == client ->
	    {WindowSize, _Limit} = Data,
	    NewState = State#state{peer_window_size = WindowSize},
	    send_rtmp(NewState, Msg#rtmp_msg{type = ?ACKSIZE,
					     data = WindowSize});
	_ ->
	    %% Others are ignored.
	    State
    end;
process_rtmp(Msg, #state{type = Type} = State) ->
    if Type == server ->
	    rtmp_session:route(State#state.session, Msg);
       Type == client ->
	    State#state.owner ! {rtmp, self(), Msg}
    end,
    State.

%%--------------------------------------------------------------------
%% Aux functions
%%--------------------------------------------------------------------
activate(Sock) ->
    inet:setopts(Sock, [{active, once}]).

handshake() ->
    crypto:rand_bytes(1528).

set_chunk_size(State, Size) ->
    NewState = send_rtmp(State,
			 #rtmp_msg{type = ?CHKSIZE,
				   stream = 0,
				   chunk_stream = 2,
				   timestamp = 0,
				   data = Size}),
    NewCodec = rtmp_codec:set_chunk_size(
		 out, Size, NewState#state.rtmp_codec),
    NewState#state{rtmp_codec = NewCodec}.

set_peer_bandwidth(State, Size, Limit) ->
    NewState = State#state{peer_window_size = Size},
    send_rtmp(NewState,
	      #rtmp_msg{type = ?PEERBW,
			stream = 0,
			chunk_stream = 2,
			timestamp = 0,
			data = {Size, Limit}}).

set_window_size(State, Size) ->
    NewState = State#state{window_size = Size},
    send_rtmp(NewState,
	      #rtmp_msg{type = ?ACKSIZE,
			stream = 0,
			chunk_stream = 2,
			timestamp = 0,
			data = Size}).

send_rtmp(State, #rtmp_msg{type = Type} = RTMP) ->
    if (Type /= ?VIDEO andalso Type /= ?AUDIO) orelse ?DBG_AV ->
	    ?DEBUG("send RTMP:~n~p", [RTMP]);
       true ->
	    ok
    end,
    Sock = State#state.sock,
    {Chunks, NewCodec} = rtmp_codec:encode(RTMP, State#state.rtmp_codec),
    tcp_send(Sock, Chunks),
    State#state{rtmp_codec = NewCodec}.

tcp_send(Sock, Data) ->
    case gen_tcp:send(Sock, Data) of
	ok ->
	    ok;
	Err ->
	    ?WARNING_MSG("socket send error: ~w", [Err]),
	    exit(normal)
    end.

%% For tests
%%tcp_send(_, _) -> ok.
