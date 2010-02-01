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
		server,
		port,
		app_name,
		type,
		audio_data,
		rtmp_sock,
		publish_stream,
		owner,
		stop_reason,
		buffer_time,
		publish_name,
		publish_rate,
		play_name}).

-define(CONNECTION_TIMEOUT, 10000).

%%====================================================================
%% API
%%====================================================================
start() ->
    spawn(fun start1/0).

start1() ->
    case oms_config:parse("stress.cfg") of
	{ok, Config} ->
	    Opts = process_config(Config),
	    p1_loglevel:set(proplists:get_value(loglevel, Opts)),
	    error_logger:add_report_handler(
	      p1_logger_h, proplists:get_value(logfile, Opts)),
	    crypto:start(),
	    erl_ddll:load_driver(".", rtmp_codec_drv),
	    start(proplists:get_value(behaviour, Opts), Opts),
	    loop();
	Err ->
	    Err
    end.

loop() ->
    receive
	_ ->
	    loop()
    end.

start('publisher-subscriber', Opts) ->
    PairNum = proplists:get_value(connection_number, Opts) div 2,
    Speed = proplists:get_value(connection_speed, Opts),
    lists:foldl(
      fun(N, Acc) ->
	      timer:sleep(Speed),
	      P1 = start_session(2*N-1, publisher, Opts),
	      P2 = start_session(2*N, publisher, Opts),
	      ?GEN_FSM:send_event(P1, {subscribe, 2*N}),
	      ?GEN_FSM:send_event(P2, {subscribe, 2*N-1}),
	      [P1, P2 | Acc]
      end, [], lists:seq(1, PairNum));
start(Type, Opts) when Type == player; Type == idle ->
    Number = proplists:get_value(connection_number, Opts),
    Speed = proplists:get_value(connection_speed, Opts),
    lists:foreach(
      fun(N) ->
	      timer:sleep(Speed),
	      start_session(N, Type, Opts)
      end, lists:seq(1, Number)).

start_session(N, Type, Opts) ->
    case ?GEN_FSM:start(?MODULE, [N, Type, self(), Opts], []) of
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
init([N, Type, Owner, Opts]) ->
    ?GEN_FSM:send_event(self(), connect),
    LifeTime = proplists:get_value(connection_lifetime, Opts),
    Rate = proplists:get_value(publish_rate, Opts),
    PlayFile = proplists:get_value(play_file, Opts),
    AppName = proplists:get_value(app_name, Opts),
    BufferTime = proplists:get_value(buffer_time, Opts),
    erlang:send_after(timer:seconds(LifeTime), self(), stop),
    State = #state{server = proplists:get_value(server, Opts),
		   port = proplists:get_value(port, Opts),
		   app_name = AppName,
		   publish_rate = Rate,
		   buffer_time = BufferTime,
		   name = N,
		   type = Type,
		   owner = Owner},
    NewState = if Type == publisher ->
		       {Size, _} = Rate,
		       Data = list_to_binary(lists:duplicate(Size, 0)),
		       State#state{audio_data = Data,
				   publish_name = integer_to_list(N)};
		  Type == subscriber ->
		       State#state{play_name = integer_to_list(N)};
		  Type == player ->
		       State#state{play_name = PlayFile};
		  Type == idle ->
		       State
	       end,
    {ok, connecting, NewState}.

connecting(connect, State) ->
    Server = State#state.server,
    Port = State#state.port,
    AppName = State#state.app_name,
    case rtmp_socket:connect(Server, Port) of
	{ok, RTMPSock} ->
	    erlang:monitor(process, RTMPSock),
	    rtmp_socket:send(RTMPSock, connect_msg(Server, Port, AppName)),
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
		BufLen = State#state.buffer_time,
		StreamName = State#state.play_name,
		rtmp_socket:send(RTMPSock, play_msg(StreamID, StreamName)),
		rtmp_socket:send(RTMPSock, buflen_msg(StreamID, BufLen)),
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
		    {_, Timeout} = State#state.publish_rate,
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
    {_, Timeout} = State#state.publish_rate,
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
handle_info(Info, StateName, State) ->
    ?INFO_MSG("unexpected info in '~p':~n\t~p", [StateName, Info]),
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
connect_msg(Server, Port, App) ->
    URI = "rtmp://" ++ Server ++ ":" ++
	integer_to_list(Port) ++ "/" ++ App,
    AMF = [{object,[{<<"app">>, App},
		    {<<"flashVer">>, <<"LNX 10,0,22,87">>},
		    {<<"swfUrl">>, undefined},
		    {<<"tcUrl">>, URI},
		    {<<"fpad">>, false},
		    {<<"capabilities">>, 15.0},
		    {<<"audioCodecs">>, 3191.0},
		    {<<"videoCodecs">>, 252.0},
		    {<<"videoFunction">>, 1.0},
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

process_config(Config) ->
    [Main|_] = oms_config:get_opts(main, Config),
    LogLevel = list_to_integer(oms_config:get_opt(loglevel, Main, "4")),
    LogFile = oms_config:get_opt(logfile, Main, "stress.log"),
    Server = oms_config:get_opt(server, Main, "localhost"),
    Port = list_to_integer(oms_config:get_opt(port, Main, "1935")),
    ConnSpeed = list_to_integer(
		  oms_config:get_opt(connection_speed, Main, "500")),
    ConnNumber = list_to_integer(
		   oms_config:get_opt(connection_number, Main, "10")),
    ConnLife = list_to_integer(
		   oms_config:get_opt(connection_lifetime, Main, "600")),
    AppName = oms_config:get_opt(app_name, Main),
    Behaviour = list_to_atom(oms_config:get_opt(behaviour, Main)),
    R = oms_config:get_opt(publish_rate, Main, "65/32"),
    Rate = case string:tokens(R, "/") of
	       [BytesStr, MSecsStr] ->
		   case catch {list_to_integer(BytesStr),
			       list_to_integer(MSecsStr)} of
		       {Bytes, MSecs} when Bytes>0, MSecs>0 ->
			   {Bytes, MSecs};
		       _ ->
			   {65, 32}
		   end;
	       _ ->
		   {65, 32}
	   end,
    BufTime = list_to_integer(oms_config:get_opt(buffer_time, Main, "65535")),
    PlayFile = oms_config:get_opt(play_file, Main),
    [{loglevel, LogLevel},
     {logfile, LogFile},
     {server, Server},
     {port, Port},
     {connection_speed, ConnSpeed},
     {connection_number, ConnNumber},
     {connection_lifetime, ConnLife},
     {publish_rate, Rate},
     {buffer_time, BufTime},
     {play_file, PlayFile},
     {app_name, AppName},
     {behaviour, Behaviour}].
