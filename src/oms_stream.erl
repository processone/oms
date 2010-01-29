%%%-------------------------------------------------------------------
%%% File    : oms_stream.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 12 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_stream).

-behaviour(gen_server).

%% API
-export([start/2, start/3, close/2, play/2, seek/3, pause/4,
	 receiveAudio/3, receiveVideo/3, route/3, start_link/3,
	 bufferTime/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("p1_logger.hrl").
-include("rtmp.hrl").
-include("oms.hrl").

-define(DICT, orddict).

-record(state, {name,
		file_stream,
		app_name,
		stream_id,
		session,
		rtmp_sock,
		players = ?DICT:new()}).

%%====================================================================
%% API
%%====================================================================
start(Ctx, Name) ->
    start(Ctx, Name, undefined).

start(Ctx, Name, FileName) ->
    supervisor:start_child(oms_stream_sup, [Ctx, Name, FileName]).

start_link(Ctx, Name, FileName) ->
    gen_server:start_link(?MODULE, [Ctx, Name, FileName], []).

close(#call_ctx{stream = StreamID}, Pid) ->
    catch gen_server:call(Pid, {close, StreamID}).

play(Ctx, Pid) ->
    {ok, CSID} = rtmp_socket:alloc_csid(Ctx#call_ctx.rtmp_sock),
    gen_server:call(Pid, {play, Ctx, CSID}).

seek(_Ctx, _Pid, _Offset) ->
    ok.

pause(_Ctx, _Pid, _Flag, _Offset) ->
    ok.

receiveAudio(_Ctx, _Pid, _Flag) ->
    ok.

receiveVideo(_Ctx, _Pid, _Flag) ->
    ok.

bufferTime(_Ctx, _Pid, _Time) ->
    ok.

route(_Ctx, Pid, RTMP) ->
    Pid ! RTMP.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([#call_ctx{stream = StreamID,
		session = Session,
		app_name = AppName,
		rtmp_sock = RTMPSock} = Ctx,
      Name, FileName]) ->
    if is_list(Name), Name /= "" ->
	    case oms_sm:add_stream(AppName, Name, self()) of
		ok ->
		    FSPid = start_file_stream(Ctx, FileName),
		    rtmp_session:stream_info(Session, StreamID,
					     ?NS_PUBLISH_START),
		    send_control(RTMPSock, StreamID, ?STREAM_BEGIN),
		    {ok, #state{name = Name,
				file_stream = FSPid,
				app_name = AppName,
				stream_id = StreamID,
				session = Session,
				rtmp_sock = RTMPSock}};
		{error, Reason} ->
		    {stop, Reason}
	    end;
       true ->
	    FSPid = start_file_stream(Ctx, FileName),
	    rtmp_session:stream_info(Session, StreamID, ?NS_PUBLISH_START),
	    send_control(RTMPSock, StreamID, ?STREAM_BEGIN),
	    {ok, #state{stream_id = StreamID,
			session = Session,
			file_stream = FSPid,
			rtmp_sock = RTMPSock}}
    end.

handle_call({close, StreamID}, _From,
	    #state{stream_id = StreamID} = State) ->
    {stop, normal, ok, State};
handle_call({close, StreamID}, _From, State) ->
    Players = State#state.players,
    NewPlayers = case ?DICT:find(StreamID, Players) of
		     {ok, {_CSID, RTMPSock, Session}} ->
			 rtmp_session:stream_info(Session, StreamID,
						  ?NS_PLAY_STOP),
			 rtmp_session:stream_info(Session, StreamID,
						  ?NS_PLAY_UNPUBLISH_NOTIFY),
			 send_control(RTMPSock, StreamID, ?STREAM_EOF),
			 ?DICT:erase(StreamID, Players);
		     _ ->
			 Players
		 end,
    {reply, ok, State#state{players = NewPlayers}};
handle_call({play, #call_ctx{rtmp_sock = RTMPSock,
			     stream = StreamID,
			     session = Session}, CSID},
	    _From, State) ->
    rtmp_session:stream_info(Session, StreamID, ?NS_PLAY_RESET),
    rtmp_session:stream_info(Session, StreamID, ?NS_PLAY_START),
    rtmp_session:stream_info(Session, StreamID, ?NS_PLAY_PUBLISH_NOTIFY),
    send_control(RTMPSock, StreamID, ?STREAM_BEGIN),
    Players = ?DICT:store(StreamID, {CSID, RTMPSock, Session},
			  State#state.players),
    {reply, ok, State#state{players = Players}};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#rtmp_msg{type = Type,
		      timestamp = TimeStamp,
		      data = Data} = RTMP,
	    #state{players = Players, file_stream = FSPid} = State) ->
    if is_pid(FSPid) ->
	    oms_file_stream:route(FSPid, RTMP);
       true ->
	    ok
    end,
    lists:foreach(
      fun({StreamID, {CSID, RTMPSock, _Session}}) ->
	      NewCSID = if Type == ?AUDIO ->
				CSID;
			   Type == ?VIDEO ->
				CSID + 2;
			   true ->
				CSID + 1
			end,
	      rtmp_socket:send(RTMPSock,
			       #rtmp_msg{type = Type,
					 stream = StreamID,
					 chunk_stream = NewCSID,
					 timestamp = TimeStamp,
					 data = Data})
      end, ?DICT:to_list(Players)),
    {noreply, State};
handle_info(Info, State) ->
    ?INFO_MSG("unexpected info:~n\t~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    Name = State#state.name,
    AppName = State#state.app_name,
    Stream = State#state.stream_id,
    ?DEBUG("stream '~s' closed", [Name]),
    catch oms_file_stream:close(State#state.file_stream),
    if AppName /= undefined ->
	    catch oms_sm:del_stream(AppName, Name);
       true ->
	    ok
    end,
    rtmp_session:stream_info(
      State#state.session, Stream, ?NS_UNPUBLISH_SUCCESS),
    send_control(State#state.rtmp_sock, Stream, ?STREAM_EOF),
    lists:foreach(
      fun({StreamID, {_CSID, RTMPSock, Session}}) ->
	      rtmp_session:stream_info(Session, StreamID, ?NS_PLAY_STOP),
	      rtmp_session:stream_info(Session, StreamID,
				       ?NS_PLAY_UNPUBLISH_NOTIFY),
	      send_control(RTMPSock, StreamID, ?STREAM_EOF)
      end, ?DICT:to_list(State#state.players)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
send_control(RTMPSock, StreamID, Type) ->
    rtmp_socket:send(RTMPSock,
		     #rtmp_msg{type = ?USER_CONTROL,
			       timestamp = 0,
			       stream = 0,
			       chunk_stream = 2,
			       data = {Type, StreamID}}).

start_file_stream(Ctx, FileName) when is_list(FileName), FileName /= "" ->
    case oms_file_stream:start(Ctx, FileName, write) of
	{ok, Pid} ->
	    Pid;
	{error, Code, Args} ->
	    rtmp_session:stream_info(Ctx#call_ctx.session,
				     Ctx#call_ctx.stream,
				     Code, Args),
	    undefined
    end;
start_file_stream(_Ctx, _FileName) ->
    undefined.
