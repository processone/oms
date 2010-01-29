%%%-------------------------------------------------------------------
%%% File    : oms_stress.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 10 Jan 2010 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_stress).

-define(GEN_FSM, gen_fsm).

-behaviour(?GEN_FSM).

%% API
-export([start/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

%% gen_fsm states
-export([connecting/2,
	 wait_for_connect_reply/2,
	 wait_for_create_stream_reply/2,
	 wait_for_onstatus_reply/2,
	 session_established/2]).

-include("rtmp.hrl").
-include("p1_logger.hrl").

-record(state, {name,
		type,
		audio_data,
		rtmp_sock,
		publish_stream,
		owner,
		stop_reason,
		publish_name,
		play_name}).

-define(SERVER, "localhost").
-define(PORT, 1935).
%%-define(APP, "oflaDemo").
-define(APP, "jingleserv").
-define(BUF_LEN_MS, 64000).
-define(CONNECTION_NUMBER, 500).
-define(CONNECTION_LIFETIME, 10). %% in minutes
-define(CONNECTION_SPEED, 200). %% in msec
-define(CONNECTION_TIMEOUT, 10000).
-define(PUBLISH_STREAM_RATE, {32, 65}). %% msec/bytes
%%-define(PUBLISH_STREAM_RATE, {50, 300}).
-define(LOGLEVEL, 4).
-define(LOGFILE, "stress.log").
-define(PLAY_FILE, "avatar-vp6.flv").

%%====================================================================
%% API
%%====================================================================
start() ->
    p1_loglevel:set(?LOGLEVEL),
    error_logger:add_report_handler(p1_logger_h, ?LOGFILE),
    crypto:start(),
    erl_ddll:load_driver(".", rtmp_codec_drv),
    %%start(idle).
    start(publisher_subscriber).
    %%start(player).

start(publisher_subscriber) ->
    PairNum = ?CONNECTION_NUMBER div 2,
    lists:foldl(
      fun(N, Acc) ->
	      timer:sleep(?CONNECTION_SPEED),
	      P1 = start_session(2*N-1, publisher),
	      P2 = start_session(2*N, publisher),
	      ?GEN_FSM:send_event(P1, {subscribe, 2*N}),
	      ?GEN_FSM:send_event(P2, {subscribe, 2*N-1}),
	      [P1, P2 | Acc]
      end, [], lists:seq(1, PairNum));
start(Type) when Type == player; Type == idle ->
    lists:foreach(
      fun(N) ->
	      timer:sleep(?CONNECTION_SPEED),
	      start_session(N, Type)
      end, lists:seq(1, ?CONNECTION_NUMBER)).

start_session(N, Type) ->
    case ?GEN_FSM:start(?MODULE, [N, Type, self()], []) of
	{ok, Pid} ->
	    receive
		{ok, Pid} ->
		    ?INFO_MSG("session '~p' started", [N]),
		    Pid;
		{error, Pid} ->
		    ?CRITICAL_MSG("session '~p' failed", [N]),
		    exit(normal)
	    after ?CONNECTION_TIMEOUT ->
		    ?CRITICAL_MSG("session start '~p' timed out", [N]),
		    exit(normal)
	    end;
	Err ->
	    ?CRITICAL_MSG("unable to start session '~p': ~p", [N, Err]),
	    exit(normal)
    end.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([N, Type, Owner]) ->
    ?GEN_FSM:send_event(self(), connect),
    erlang:send_after(timer:minutes(?CONNECTION_LIFETIME), self(), stop),
    State = #state{name = N, type = Type, owner = Owner},
    NewState = if Type == publisher ->
		       {_, Size} = ?PUBLISH_STREAM_RATE,
		       Data = list_to_binary(lists:duplicate(Size, 0)),
		       State#state{audio_data = Data,
				   publish_name = integer_to_list(N)};
		  Type == subscriber ->
		       State#state{play_name = integer_to_list(N)};
		  Type == player ->
		       State#state{play_name = ?PLAY_FILE};
		  Type == idle ->
		       State
	       end,
    {ok, connecting, NewState}.

connecting(connect, State) ->
    case rtmp_socket:connect(?SERVER, ?PORT) of
	{ok, RTMPSock} ->
	    rtmp_socket:send(RTMPSock, connect_msg()),
	    {next_state, wait_for_connect_reply,
	     State#state{rtmp_sock = RTMPSock}};
	Err ->
	    ?ERROR_MSG("connection failed: ~p", [Err]),
	    {stop, normal, State}
    end;
connecting(_Event, State) ->
    {stop, normal, State}.

wait_for_connect_reply(#rtmp_msg{type = ?AMF0_CMD}, State) ->
    if State#state.type == idle ->
	    State#state.owner ! {ok, self()},
	    {next_state, session_established, State};
       true ->
	    rtmp_socket:send(State#state.rtmp_sock, createStream_msg()),
	    {next_state, wait_for_create_stream_reply, State}
    end;
wait_for_connect_reply(Event, State) ->
    ?WARNING_MSG("unexpected event in 'wait_for_connect_reply':~n\t~p",
		 [Event]),
    {next_state, wait_for_connect_reply, State}.

wait_for_create_stream_reply(
  #rtmp_msg{type = ?AMF0_CMD,
	    data = #amf_cmd{name = <<"_result">>,
			    body = [null, StreamID]}}, State) ->
    RTMPSock = State#state.rtmp_sock,
    NewState =
	case State#state.type of
	    publisher ->
		StreamName = State#state.publish_name,
		rtmp_socket:send(RTMPSock, publish_msg(StreamID, StreamName)),
		State#state{publish_stream = trunc(StreamID)};
	    subscriber ->
		StreamName = State#state.play_name,
		rtmp_socket:send(RTMPSock, play_msg(StreamID, StreamName)),
		rtmp_socket:send(RTMPSock, buflen_msg(StreamID, 0)),
		State;
	    player ->
		StreamName = State#state.play_name,
		rtmp_socket:send(RTMPSock, play_msg(StreamID, StreamName)),
		rtmp_socket:send(RTMPSock, buflen_msg(StreamID, ?BUF_LEN_MS)),
		State
	end,
    {next_state, wait_for_onstatus_reply, NewState};
wait_for_create_stream_reply(Event, State) ->
    ?WARNING_MSG("unexpected event in 'wait_for_create_stream_reply':~n\t~p",
		 [Event]),
    {next_state, wait_for_create_stream_reply, State}.

wait_for_onstatus_reply(
  #rtmp_msg{type = ?AMF0_CMD,
	    data = #amf_cmd{body = [null, {object, Obj}]} = AMFCmd},
  #state{type = Type, owner = Owner} = State) ->
    case lists:keysearch(<<"level">>, 1, Obj) of
	{value, {_, <<"status">>}} ->
	    if Type == publisher ->
		    {Timeout, _} = ?PUBLISH_STREAM_RATE,
		    erlang:send_after(Timeout, self(), send_audio);
	       true ->
		    ok
	    end,
	    Owner ! {ok, self()},
	    {next_state, session_established, State};
	_ ->
	    ?WARNING_MSG("got invalid onstatus:~n\t~p", [AMFCmd]),
	    {stop, normal, State}
    end;
wait_for_onstatus_reply(Event, State) ->
    ?WARNING_MSG("unexpected event in 'wait_for_onstatus_reply':~n\t~p",
		 [Event]),
    {next_state, wait_for_onstatus_reply, State}.

session_established({subscribe, StreamName}, State) ->
    rtmp_socket:send(State#state.rtmp_sock, createStream_msg()),
    {next_state, wait_for_create_stream_reply,
     State#state{type = subscriber,
		 play_name = integer_to_list(StreamName)}};
session_established(_Event, State) ->
    {next_state, session_established, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

handle_info(stop, _StateName, State) ->
    {stop, normal, State#state{stop_reason = normal}};
handle_info(send_audio, StateName, State) ->
    {Timeout, _} = ?PUBLISH_STREAM_RATE,
    erlang:send_after(Timeout, self(), send_audio),
    rtmp_socket:send(State#state.rtmp_sock,
		     #rtmp_msg{type = ?AUDIO,
			       stream = State#state.publish_stream,
			       data = State#state.audio_data,
			       chunk_stream = 5,
			       timestamp = 0}),
    {next_state, StateName, State};
handle_info({rtmp, _, #rtmp_msg{type = ?USER_CONTROL,
				data = Control} = Msg},
	    StateName, State) ->
    case Control of
	{?PING_REQUEST, _TimeStamp} ->
	    Reply = Msg#rtmp_msg{data = {?PING_RESPONSE, 0}},
	    rtmp_socket:send(State#state.rtmp_sock, Reply);
	_ ->
	    ok
    end,
    {next_state, StateName, State};
handle_info({rtmp, _, Msg}, StateName, State) ->
    ?GEN_FSM:send_event(self(), Msg),
    {next_state, StateName, State};
handle_info({rtmp_closed, _}, _StateName, State) ->
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName,
	  #state{name = Name, owner = Owner, stop_reason = StopReason}) ->
    if StopReason /= normal ->
	    Owner ! {error, self()},
	    ?WARNING_MSG("connection '~p' terminated", [Name]);
       true ->
	    ok
    end.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
connect_msg() ->
    URI = "rtmp://" ++ ?SERVER ++ "/" ++ ?APP,
    AMF = [{object,[{<<"app">>,<<?APP>>},
		    {<<"flashVer">>,<<"LNX 10,0,22,87">>},
		    {<<"swfUrl">>,undefined},
		    {<<"tcUrl">>, URI},
		    {<<"fpad">>,false},
		    {<<"capabilities">>,15.0},
		    {<<"audioCodecs">>,3191.0},
		    {<<"videoCodecs">>,252.0},
		    {<<"videoFunction">>,1.0},
		    {<<"pageUrl">>,undefined}]}],
    #rtmp_msg{type = ?AMF0_CMD,
	      stream = 0,
	      timestamp = 0,
	      chunk_stream = 3,
	      data = #amf_cmd{name = <<"connect">>,
			      trid = 1.0,
			      body = AMF}}.

createStream_msg() ->
    #rtmp_msg{type = ?AMF0_CMD,
	      stream = 0,
	      timestamp = 0,
	      chunk_stream = 3,
	      data = #amf_cmd{name = <<"createStream">>,
			      trid = 2.0,
			      body = [null]}}.

play_msg(StreamID, StreamName) ->
    #rtmp_msg{type = ?AMF0_CMD,
	      stream = trunc(StreamID),
	      timestamp = 0,
	      chunk_stream = 3,
	      data = #amf_cmd{name = <<"play">>,
			      trid = 3.0,
			      body = [null, StreamName]}}.

publish_msg(StreamID, StreamName) ->
    #rtmp_msg{type = ?AMF0_CMD,
	      stream = trunc(StreamID),
	      timestamp = 0,
	      chunk_stream = 3,
	      data = #amf_cmd{name = <<"publish">>,
			      trid = 3.0,
			      body = [null, StreamName]}}.

buflen_msg(StreamID, BufLen) ->
    #rtmp_msg{type = ?USER_CONTROL,
	      stream = 0,
	      chunk_stream = 2,
	      timestamp = 0,
	      data = {?SET_BUFFER_LENGTH, trunc(StreamID), BufLen}}.
