%%%-------------------------------------------------------------------
%%% File    : oms_file_stream.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 26 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_file_stream).

-define(GEN_FSM, gen_fsm).

-behaviour(?GEN_FSM).

%% API
-export([start/3, close/1, close/2, start_link/3,
	 play/2, seek/3, pause/4, receiveAudio/3,
	 receiveVideo/3, route/2, route/3, bufferTime/3]).

%% gen_fsm states
-export([playing/2,
	 paused/2,
	 recording/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

-include("rtmp.hrl").
-include("p1_logger.hrl").
-include("oms.hrl").

-record(state, {rtmp_sock,
		flv_codec,
		timestamp,
		type,
		stream,
		session,
		send_audio = true,
		send_video = true,
		tref,
		filename,
		chunk_stream}).

%%====================================================================
%% API
%%====================================================================
start(Ctx, Path, Type) when Type == read; Type == write ->
    case supervisor:start_child(oms_file_stream_sup, [Ctx, Path, Type]) of
	{ok, Pid} ->
	    {ok, Pid};
	{error, _Reason} when Type == read ->
	    {error, ?NS_PLAY_STREAM_NOT_FOUND, []};
	{error, _Reason} when Type == write ->
	    {error, ?NS_RECORD_FAILED, []}
    end.

close(_Ctx, Pid) ->
    close(Pid).

close(Pid) ->
    ?GEN_FSM:send_all_state_event(Pid, stop).

start_link(Ctx, Path, Type) ->
    ?GEN_FSM:start_link(?MODULE, [Ctx, Path, Type], []).

play(#call_ctx{stream = StreamID, rtmp_sock = RTMPSock}, Pid) ->
    {ok, CSID} = rtmp_socket:alloc_csid(RTMPSock),
    ?GEN_FSM:send_all_state_event(Pid, {play, CSID, StreamID}).

seek(_Ctx, Pid, MilliSecs) ->
    ?GEN_FSM:send_event(Pid, {seek, MilliSecs}).

pause(_Ctx, Pid, true, Offset) ->
    ?GEN_FSM:send_event(Pid, {pause, Offset});
pause(_Ctx, Pid, false, Offset) ->
    ?GEN_FSM:send_event(Pid, {resume, Offset}).

route(_Ctx, Pid, RTMP) ->
    route(Pid, RTMP).

route(Pid, RTMP) ->
    ?GEN_FSM:send_event(Pid, RTMP).

receiveAudio(_Ctx, Pid, Flag) ->
    ?GEN_FSM:send_all_state_event(Pid, {receiveAudio, Flag}).

receiveVideo(_Ctx, Pid, Flag) ->
    ?GEN_FSM:send_all_state_event(Pid, {receiveVideo, Flag}).

bufferTime(_Ctx, Pid, Time) ->
    ?GEN_FSM:send_all_state_event(Pid, {bufferTime, Time}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([#call_ctx{rtmp_sock = RTMPSock,
		session = Session,
		stream = Stream},
      Path, Type]) ->
    case flv_codec:open(Path, [Type]) of
	{ok, Codec} when Type == read ->
	    {ok, playing, #state{rtmp_sock = RTMPSock,
				 type = Type,
				 session = Session,
				 stream = Stream,
				 filename = Path,
				 flv_codec = Codec}};
	{ok, Codec} when Type == write ->
	    rtmp_session:stream_info(Session, Stream, ?NS_RECORD_START),
	    {ok, recording, #state{type = Type,
				   session = Session,
				   stream = Stream,
				   filename = Path,
				   flv_codec = Codec}};
	{error, Reason} = Err ->
	    log_error(Path, Err),
	    {stop, Reason}
    end.

playing(read_and_send, State) ->
    case flv_codec:read(State#state.flv_codec) of
	{ok, {Type, TimeStamp, Data}, NewCodec} ->
	    send(State, Type, TimeStamp, Data),
	    TRef = ?GEN_FSM:send_event_after(10, read_and_send),
	    {next_state, playing,
	     State#state{flv_codec = NewCodec, tref = TRef}};
	eof ->
	    send_info(State, ?NS_PLAY_STOP),
	    send_control(State, ?STREAM_EOF),
	    {next_state, playing, State};
	Err ->
	    log_error(State#state.filename, Err),
	    send_info(State, ?NS_PLAY_FAILED),
	    send_control(State, ?STREAM_EOF),
	    {next_state, playing, State}
    end;
playing({seek, OffSet}, State) ->
    cancel_timer(State),
    case flv_codec:read(State#state.flv_codec, OffSet) of
	{ok, {Type, TimeStamp, Data}, NewCodec} ->
	    send_info(State, ?NS_SEEK_NOTIFY),
	    send_control(State, ?STREAM_BEGIN),
	    send(State, Type, TimeStamp, Data),
	    TRef = ?GEN_FSM:send_event_after(10, read_and_send),
	    {next_state, playing,
	     State#state{flv_codec = NewCodec, tref = TRef}};
	eof ->
	    send_info(State, ?NS_SEEK_NOTIFY),
	    send_control(State, ?STREAM_EOF),
	    {next_state, playing, State};
	Err ->
	    log_error(State#state.filename, Err),
	    send_info(State, ?NS_SEEK_FAILED),
	    send_control(State, ?STREAM_EOF),
	    {next_state, playing, State}
    end;
playing({pause, _OffSet}, State) ->
    cancel_timer(State),
    send_info(State, ?NS_PAUSE_NOTIFY),
    {next_state, paused, State};
playing(Event, State) ->
    ?INFO_MSG("unexpected event in 'playing': ~p", [Event]),
    {next_state, playing, State}.

paused({resume, _OffSet}, State) ->
    cancel_timer(State),
    send_info(State, ?NS_UNPAUSE_NOTIFY),
    TRef = ?GEN_FSM:send_event_after(0, read_and_send),
    {next_state, playing, State#state{tref = TRef}};
paused({seek, OffSet}, State) ->
    cancel_timer(State),
    case flv_codec:read(State#state.flv_codec, OffSet) of
	{ok, {Type, TimeStamp, Data}, NewCodec} ->
	    send_info(State, ?NS_SEEK_NOTIFY),
	    send_control(State, ?STREAM_BEGIN),
	    send(State, Type, TimeStamp, Data),
	    {next_state, paused, State#state{flv_codec = NewCodec}};
	eof ->
	    send_info(State, ?NS_SEEK_NOTIFY),
	    send_control(State, ?STREAM_EOF),
	    {next_state, paused, State};
	Err ->
	    log_error(State#state.filename, Err),
	    send_info(State, ?NS_SEEK_FAILED),
	    send_control(State, ?STREAM_EOF),
	    {next_state, paused, State}
    end;
paused(Event, State) ->
    ?INFO_MSG("unexpected event in 'paused': ~p", [Event]),
    {next_state, paused, State}.

recording(#rtmp_msg{type = Type, data = Data}, State) ->
    {TimeStamp, StartTimeStamp} =
	case State#state.timestamp of
	    undefined ->
		{0, timestamp()};
	    TS ->
		{timestamp() - TS, TS}
	end,
    case flv_codec:write(State#state.flv_codec,
			 Type, TimeStamp, Data) of
	{ok, NewCodec} ->
	    {next_state, recording,
	     State#state{flv_codec = NewCodec,
			 timestamp = StartTimeStamp}};
	Err ->
	    log_error(State#state.filename, Err),
	    send_info(State, ?NS_RECORD_FAILED),
	    {stop, normal, State}
    end;
recording(_, State) ->
    {next_state, recording, State}.

handle_event({play, CSID, Stream}, StateName,
	     #state{type = read} = State) ->
    cancel_timer(State),
    case flv_codec:position(State#state.flv_codec, 0) of
	{ok, NewCodec} ->
	    NewState = State#state{stream = Stream,
				   chunk_stream = CSID,
				   flv_codec = NewCodec},
	    send_info(NewState, ?NS_PLAY_RESET),
	    send_info(NewState, ?NS_PLAY_START),
	    send_control(NewState, ?STREAM_IS_RECORDED),
	    send_control(NewState, ?STREAM_BEGIN),
	    TRef = ?GEN_FSM:send_event_after(0, read_and_send),
	    {next_state, playing, NewState#state{tref = TRef}};
	Err ->
	    log_error(State#state.filename, Err),
	    send_info(State, ?NS_PLAY_FAILED),
	    send_control(State, ?STREAM_EOF),
	    {next_state, StateName, State}
    end;
handle_event({receiveAudio, Flag}, StateName, State) ->
    {next_state, StateName, State#state{send_audio = Flag}};
handle_event({receiveVideo, Flag}, StateName, State) ->
    {next_state, StateName, State#state{send_video = Flag}};
handle_event({bufferTime, _Time}, StateName, State) ->
    %% TODO: adjust play jitter here
    {next_state, StateName, State};
handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    ?INFO_MSG("unexpected event in '~s': ~p", [StateName, Event]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    flv_codec:close(State#state.flv_codec).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
send_control(#state{rtmp_sock = RTMPSock, stream = StreamID}, Type) ->
    rtmp_socket:send(RTMPSock,
		     #rtmp_msg{type = ?USER_CONTROL,
			       timestamp = 0,
			       stream = 0,
			       chunk_stream = 2,
			       data = {Type, StreamID}}).

send_info(#state{session = Session, stream = StreamID}, Code) ->
    rtmp_session:stream_info(Session, StreamID, Code).

send(#state{stream = Stream,
	    send_audio = SendAudio,
	    send_video = SendVideo,
	    rtmp_sock = RTMPSock,
	    chunk_stream = CSID}, Type, TimeStamp, Data) ->
    SendFlag = if Type == ?AUDIO andalso SendAudio ->
		       true;
		  Type == ?VIDEO andalso SendVideo ->
		       true;
		  Type == ?AMF0_DATA ->
		       true;
		  true ->
		       false
	       end,
    if SendFlag ->
	    NewCSID = if Type == ?AUDIO ->
			      CSID;
			 Type == ?VIDEO ->
			      CSID + 1;
			 true ->
			      CSID + 2
		      end,
	    rtmp_socket:sync_send(RTMPSock,
				  #rtmp_msg{type = Type,
					    timestamp = TimeStamp,
					    stream = Stream,
					    chunk_stream = NewCSID,
					    data = Data});
       true ->
	    ok
    end.

timestamp() ->
    %% Unix timestamp in millisec
    timer:now_diff(now(), {0, 0, 0}) div 1000.

log_error(FileName, {error, Why}) ->
    ?WARNING_MSG("error in FLV file ~p: ~s",
		 [FileName, flv_codec:format_error(Why)]).

cancel_timer(#state{tref = undefined}) ->
    false;
cancel_timer(State) ->
    ?GEN_FSM:cancel_timer(State#state.tref).
