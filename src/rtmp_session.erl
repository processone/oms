%%%-------------------------------------------------------------------
%%% File    : rtmp_session.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created :  4 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(rtmp_session).

-define(GEN_FSM, gen_fsm).

-behaviour(?GEN_FSM).

%% API
-export([start/2, stop/1, start_link/2, route/2,
	 amf_call/3, amf_call/4, stream_info/3, stream_info/4]).

%% gen_fsm states
-export([wait_for_connect/2,
	 session_established/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

-include("p1_logger.hrl").
-include("rtmp.hrl").
-include("oms.hrl").

-define(SETS, sets).
-define(DICT, dict).

-record(state, {rtmp_sock,
		addr,
		app_name,
		app_options,
		streams = ?DICT:new(),
		sos = ?SETS:new(),
		pending_players = ?DICT:new(),
		connection_mod,
		stream_mod}).

%%====================================================================
%% API
%%====================================================================
start(RTMPSock, Addr) ->
    supervisor:start_child(oms_rtmp_session_sup, [RTMPSock, Addr]).

stop(Pid) ->
    ?GEN_FSM:send_all_state_event(Pid, stop).

start_link(RTMPSock, Addr) ->
    ?GEN_FSM:start_link(?MODULE, [RTMPSock, Addr], []).

route(Pid, Msg) ->
    ?GEN_FSM:send_event(Pid, Msg).

amf_call(Pid, Fun, Args) ->
    amf_call(Pid, Fun, Args, 0).

amf_call(Pid, Fun, Args, Stream) ->
    ?GEN_FSM:send_event(Pid, {amf_call, Fun, Args, Stream}).

stream_info(Session, Stream, Code) ->
    stream_info(Session, Stream, Code, []).

stream_info(Session, Stream, Code, KeyVals)
  when is_pid(Session), is_integer(Stream), Stream > 0,
       is_list(Code), is_list(KeyVals) ->
    Object = {object, [{"code", Code},
		       {"level", info_level(Code)}
		       | KeyVals]},
    amf_call(Session, onStatus, [Object], Stream).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([RTMPSock, Addr]) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    {ok, wait_for_connect, #state{rtmp_sock = RTMPSock, addr = Addr}}.

wait_for_connect(#rtmp_msg{data = AMF} = Msg, State) ->
    case AMF of
	#amf_cmd{name = <<"connect">>,
		 body = [{object, Obj} | _]} ->
	    AppName = get_app_name(Obj),
	    case oms_app_mgr:get_application(AppName) of
		{ok, ConnectionMod, StreamMod, AppOpts} ->
		    ?INFO_MSG("connection request for application '~s'"
			      " from ~w",
			      [AppName, State#state.addr]),
		    NewState1 = State#state{
				  app_name = AppName,
				  app_options = AppOpts,
				  connection_mod = ConnectionMod,
				  stream_mod = StreamMod},
		    case process_amf(NewState1, Msg) of
			{ok, NewState2} ->
			    {next_state, session_established, NewState2};
			_Err ->
			    {stop, normal, State}
		    end;
		_ ->
		    ?WARNING_MSG("dropping request for unknown "
				 "application '~s' from ~w",
				 [AppName, State#state.addr]),
		    {stop, normal, State}
	    end;
	_ ->
	    {stop, normal, State}
    end;
wait_for_connect(Event, State) ->
    ?INFO_MSG("unexpected event in wait_for_connect:~n\t~p", [Event]),
    {next_state, wait_for_connect, State}.

session_established(#rtmp_msg{type = Type} = Msg, State)
  when Type == ?AMF0_CMD; Type == ?AMF3_CMD ->
    case process_amf(State, Msg) of
	{ok, NewState} ->
	    {next_state, session_established, NewState};
	_ ->
	    {stop, normal, State}
    end;
session_established(#rtmp_msg{type = Type} = Msg, State)
  when Type == ?AMF0_SO; Type == ?AMF3_SO ->
    case process_so(State, Msg) of
	{ok, NewState} ->
	    {next_state, session_established, NewState};
	_ ->
	    {stop, normal, State}
    end;
session_established(#rtmp_msg{type = ?USER_CONTROL} = Msg, State) ->
    case Msg#rtmp_msg.data of
	{?SET_BUFFER_LENGTH, StreamID, MilliSecs} ->
	    case ?DICT:find(StreamID, State#state.streams) of
		{ok, {Mod, StreamRef, _Type}} ->
		    Ctx = make_context(State, StreamID),
		    Mod:bufferTime(Ctx, StreamRef, MilliSecs);
		error ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {next_state, session_established, State};
session_established(#rtmp_msg{stream = StreamID} = Msg, State) ->
    case ?DICT:find(StreamID, State#state.streams) of
	{ok, {Mod, StreamRef, _Type}} ->
	    Ctx = make_context(State, StreamID),
	    Mod:route(Ctx, StreamRef, Msg);
	error ->
	    ok
    end,
    {next_state, session_established, State};
session_established({amf_call, Fun, Args, Stream}, State) ->
    Name = list_to_binary(atom_to_list(Fun)),
    Cmd = #amf_cmd{name = Name,
		   trid = 0,
		   body = [null | Args]},
    rtmp_socket:send(State#state.rtmp_sock,
		     #rtmp_msg{type = ?AMF0_CMD,
			       stream = Stream,
			       chunk_stream = 3,
			       timestamp = 0,
			       data = Cmd}),
    {next_state, session_established, State};
session_established(SO, State) when is_record(SO, amf_so) ->
    rtmp_socket:send(State#state.rtmp_sock,
		     #rtmp_msg{type = ?AMF0_SO,
			       stream = 0,
			       chunk_stream = 3,
			       timestamp = 0,
			       data = SO}),
    {next_state, session_established, State};
session_established({activate_pending_players, Name, StreamRef}, State) ->
    PendingPlayers = State#state.pending_players,
    Streams =
	lists:foldl(
	  fun({StreamID, {Mod, StreamName}}, Acc)
	     when StreamName == Name ->
		  ?DICT:store(StreamID, {Mod, StreamRef, play}, Acc);
	     (_, Acc) ->
		  Acc
	  end, State#state.streams, ?DICT:to_list(PendingPlayers)),
    NewPendingPlayers =
	?DICT:filter(
	   fun(StreamID, {Mod, StreamName}) when Name == StreamName ->
		   Ctx = make_context(State, StreamID),
		   Mod:play(Ctx, StreamRef),
		   call(State#state.connection_mod, stream_started,
			[Ctx, StreamRef, play]),
		   false;
	      (_, _) ->
		   true
	   end, PendingPlayers),
    {next_state, session_established,
     State#state{pending_players = NewPendingPlayers, streams = Streams}};
session_established(Event, State) ->
    ?INFO_MSG("unexpected event in session_established:~n\t~p", [Event]),
    {next_state, session_established, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

handle_info(_Info, StateName, State) ->
    ?INFO_MSG("unexpected info in ~p:~n\t~p", [StateName, _Info]),
    {next_state, StateName, State}.

terminate(_Reason, StateName, State) ->
    RTMPSock = State#state.rtmp_sock,
    ConnectionMod = State#state.connection_mod,
    catch rtmp_socket:close(RTMPSock),
    if StateName == session_established ->
	    catch oms_sm:del_session(State#state.app_name, self()),
	    ?DICT:filter(
	       fun(StreamID, {Mod, StreamRef, Type}) ->
		       Ctx = make_context(State, StreamID),
		       catch Mod:close(Ctx, StreamRef),
		       catch ConnectionMod:stream_stopped(
			       Ctx, StreamRef, Type),
		       false
	       end, State#state.streams),
	    ?SETS:filter(
	       fun(Name) ->
		       catch oms_so_mgr:close(State#state.app_name,
					      self(), Name),
		       false
	       end, State#state.sos),
	    catch (ConnectionMod:close(make_context(State)));
       true ->
	    ok
    end,
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
get_app_name(Obj) ->
    case lists:keysearch(<<"tcUrl">>, 1, Obj) of
	{value, {_, AppURL}} ->
	    case string:tokens(to_string(AppURL), "/") of
		[_, _, AppName] ->
		    AppName;
		_ ->
		    ""
	    end;
	_ ->
	    ""
    end.

%% NetConnection methods:
%% connect | call | createStream | deleteStream
process_amf(#state{connection_mod = Mod} = State,
	    #rtmp_msg{stream = 0,
		      data = #amf_cmd{name = <<"connect">>} = Cmd} = Msg) ->
    RTMPSock = State#state.rtmp_sock,
    rtmp_socket:setopts(RTMPSock,
			[{window_size, ?DEFAULT_WINDOW_SIZE},
			 {peer_window_size, ?DEFAULT_WINDOW_SIZE}]),
    rtmp_socket:send(RTMPSock,
		     #rtmp_msg{type = ?USER_CONTROL,
			       stream = 0,
			       chunk_stream = 2,
			       timestamp = 0,
			       data = {?STREAM_BEGIN, 0}}),
    Ctx = make_context(State),
    Cmd = #amf_cmd{body = [{object, Obj} | Args]} = Msg#rtmp_msg.data,
    case call(Mod, connect, [Ctx | Args]) of
	ok ->
	    Info = info(?NC_CONNECT_SUCCESS),
	    ResAMF =
		case lists:keysearch(<<"objectEncoding">>, 1, Obj) of
		    {value, {_, Encoding}} when is_number(Encoding) ->
			{ecma_array,
			 [{"objectEncoding", Encoding},
			  {"application", null} | Info]};
		    _ ->
			{object, Info}
		end,
	    ResCmd = Cmd#amf_cmd{name = result, body = [null, ResAMF]},
	    case oms_sm:add_session(State#state.app_name, self(),
				    State#state.addr) of
		ok ->
		    rtmp_socket:send(RTMPSock,
				     Msg#rtmp_msg{timestamp = 0,
						  data = ResCmd}),
		    {ok, State};
		_Err ->
		    stop
	    end;
	error ->
	    ResAMF = {object, info(?NC_CONNECT_REJECTED)},
	    ResCmd = Cmd#amf_cmd{name = error, body = [null, ResAMF]},
	    rtmp_socket:send(RTMPSock,
			     Msg#rtmp_msg{timestamp = 0, data = ResCmd}),
	    stop;
	{error, Code, Args} ->
	    ResAMF = {object, info(Code, Args)},
	    ResCmd = Cmd#amf_cmd{name = error, body = [null, ResAMF]},
	    rtmp_socket:send(RTMPSock,
			     Msg#rtmp_msg{timestamp = 0, data = ResCmd}),
	    stop;
	Bad ->
	    erlang:error({bad_api_return, Bad})
    end;
process_amf(State,
	    #rtmp_msg{stream = 0,
		      data = #amf_cmd{name = <<"createStream">>}} = Msg) ->
    Cmd = Msg#rtmp_msg.data,
    StreamID = random:uniform(1 bsl 16),
    ResCmd = Cmd#amf_cmd{name = result,
			 body = [null, StreamID]},
    rtmp_socket:send(State#state.rtmp_sock,
		     Msg#rtmp_msg{timestamp = 0, data = ResCmd}),
    {ok, State};
process_amf(State,
	    #rtmp_msg{data = #amf_cmd{name = <<"deleteStream">>}}) ->
    %%TODO: not sure what to do here
    {ok, State};
%% NetStream methods:
%% play | closeStream | receiveAudio |
%% receiveVideo | publish | seek | pause
process_amf(#state{rtmp_sock = RTMPSock} = State,
	    #rtmp_msg{data = #amf_cmd{name = <<"play">>,
				      body = [_, NameBin]} = Cmd} = Msg)
  when is_binary(NameBin) ->
    Name = to_string(NameBin),
    StreamID = Msg#rtmp_msg.stream,
    Ctx = make_context(State, StreamID),
    Res = case ?DICT:find(StreamID, State#state.streams) of
	      {ok, {StreamMod, Ref, _Type}} ->
		  {ok, StreamMod, Ref};
	      _ ->
		  case call(State#state.connection_mod, play, [Ctx, Name]) of
		      {play_file, FileName} ->
			  case oms_file_stream:start(Ctx, FileName, read) of
			      {ok, Pid} ->
				  {ok, oms_file_stream, Pid};
			      Err ->
				  Err
			  end;
		      {play, StreamName} ->
			  AppName = State#state.app_name,
			  case oms_sm:find_stream(AppName, StreamName) of
			      {ok, Pid} ->
				  {ok, oms_stream, Pid};
			      {error, enoent} ->
				  {ok, oms_stream, pending_play};
			      _ ->
				  {error, ?NS_PLAY_STREAM_NOT_FOUND, []}
			  end;
		      {stream, Ref} ->
			  {ok, State#state.connection_mod, Ref};
		      error ->
			  {error, ?NS_PLAY_STREAM_NOT_FOUND, []};
		      {error, _Code, _Args} = Err ->
			  Err;
		      Bad ->
			  erlang:error({bad_api_return, Bad})
		  end
	  end,
    case Res of
	{error, Code, KeyVals} ->
	    AMF = {object, info(Code, KeyVals)},
	    ResCmd = Cmd#amf_cmd{name = status, body = [null, AMF]},
	    rtmp_socket:send(RTMPSock,
			     Msg#rtmp_msg{timestamp = 0, data = ResCmd}),
	    {ok, State};
	{ok, Mod, pending_play} ->
	    %% WARNING: dirty hack to work around race condition
	    %% in publish-subscribe relations. We cache
	    %% pending play()s and activate them when
	    %% publisher is ready
	    ?DEBUG("got pending play request for ~p", [Name]),
	    PendingPlayers = ?DICT:store(
				StreamID, {Mod, Name},
				State#state.pending_players),
	    {ok, State#state{pending_players = PendingPlayers}};
	{ok, Mod, StreamRef} ->
	    Mod:play(Ctx, StreamRef),
	    call(State#state.connection_mod,
		 stream_started, [Ctx, StreamRef, play]),
	    Streams = ?DICT:store(
			 StreamID, {Mod, StreamRef, play},
			 State#state.streams),
	    {ok, State#state{streams = Streams}}
    end;
process_amf(_State,
	    #rtmp_msg{data = #amf_cmd{name = <<"play">>}}) ->
    stop;
process_amf(State,
	    #rtmp_msg{stream = StreamID,
		      data = #amf_cmd{name = <<"seek">>} = Cmd} = Msg) ->
    case Cmd#amf_cmd.body of
	[_, Offset] when is_number(Offset) ->
	    case ?DICT:find(StreamID, State#state.streams) of
		{ok, {Mod, StreamRef, _Type}} ->
		    Ctx = make_context(State, StreamID),
		    Mod:seek(Ctx, StreamRef, Offset);
		_ ->
		    Err = {object, info(?NS_SEEK_FAILED)},
		    ResCmd = Cmd#amf_cmd{name = error,
					 body = [null, Err]},
		    rtmp_socket:send(State#state.rtmp_sock,
				     Msg#rtmp_msg{timestamp = 0,
						  data = ResCmd})
	    end,
	    {ok, State};
	_  ->
	    stop
    end;
process_amf(State,
	    #rtmp_msg{stream = StreamID,
		      data = #amf_cmd{name = Name} = Cmd} = Msg)
  when Name == <<"pause">>; Name == <<"pauseRaw">> ->
    case Cmd#amf_cmd.body of
	[_, Flag, Offset] when is_boolean(Flag), is_number(Offset) ->
	    case ?DICT:find(StreamID, State#state.streams) of
		{ok, {Mod, StreamRef, _Type}} ->
		    Ctx = make_context(State, StreamID),
		    Mod:pause(Ctx, StreamRef, Flag, Offset);
		_ ->
		    Err = {object, info(?NS_PLAY_FAILED)},
		    ResCmd = Cmd#amf_cmd{name = error, body = [null, Err]},
		    rtmp_socket:send(State#state.rtmp_sock,
				     Msg#rtmp_msg{timestamp = 0,
						  data = ResCmd})
	    end,
	    {ok, State};
	_ ->
	    stop
    end;
process_amf(#state{rtmp_sock = RTMPSock} = State,
	    #rtmp_msg{data = #amf_cmd{body = [_ | PubArgs],
				      name = <<"publish">>} = Cmd} = Msg) ->
    Args = get_publish_args(PubArgs),
    StreamID = Msg#rtmp_msg.stream,
    Ctx = make_context(State, StreamID),
    Res = case call(State#state.connection_mod, publish, [Ctx | Args]) of
	      {publish, StreamName} ->
		  case oms_stream:start(Ctx, StreamName) of
		      {ok, Pid} ->
			  {ok, oms_stream, StreamName, Pid};
		      _Err ->
			  {error, ?NS_PUBLISH_BAD_NAME, []}
		  end;
	      {publish, StreamName, RecordFileName} ->
		  case oms_stream:start(Ctx, StreamName, RecordFileName) of
		      {ok, Pid} ->
			  {ok, oms_stream, StreamName, Pid};
		      _Err ->
			  {error, ?NS_PUBLISH_BAD_NAME, []}
		  end;
	      {stream, Ref} ->
		  [StreamName, _] = Args,
		  {ok, State#state.connection_mod, StreamName, Ref};
	      error ->
		  {error, ?NS_PUBLISH_BAD_NAME, []};
	      {error, _Code, _Args} = Err ->
		  Err;
	      Bad ->
		  erlang:error({bad_api_return, Bad})
	  end,
    case Res of
	{error, Code, KeyVals} ->
	    ErrAMF = {object, info(Code, KeyVals)},
	    ResCmd = Cmd#amf_cmd{name = status, body = [null, ErrAMF]},
	    rtmp_socket:send(
	      RTMPSock, Msg#rtmp_msg{timestamp = 0, data = ResCmd}),
	    {ok, State};
	{ok, Mod, Name, StreamRef} ->
	    call(State#state.connection_mod,
		 stream_started, [Ctx, StreamRef, publish]),
	    lists:foreach(
	      fun({Session, _}) ->
		      ?GEN_FSM:send_event(
			 Session,
			 {activate_pending_players, Name, StreamRef})
	      end, oms_sm:all_sessions(State#state.app_name)),
	    NewStreams = ?DICT:store(StreamID, {Mod, StreamRef, publish},
				     State#state.streams),
	    {ok, State#state{streams = NewStreams}}
    end;
process_amf(_State,
	    #rtmp_msg{data = #amf_cmd{name = <<"publish">>}}) ->
    stop;
process_amf(#state{streams = Streams} = State,
	    #rtmp_msg{data = #amf_cmd{name = <<"closeStream">>},
		      stream = StreamID}) ->
    case ?DICT:find(StreamID, Streams) of
	{ok, {Mod, StreamRef, Type}} ->
	    Ctx = make_context(State, StreamID),
	    NewStreams = ?DICT:erase(StreamID, Streams),
	    Mod:close(Ctx, StreamRef),
	    call(State#state.connection_mod,
		 stream_stopped, [Ctx, StreamRef, Type]),
	    {ok, State#state{streams = NewStreams}};
	_ ->
	    PendingPlayers = ?DICT:erase(StreamID, State#state.pending_players),
	    {ok, State#state{pending_players = PendingPlayers}}
    end;
process_amf(#state{streams = Streams} = State,
	    #rtmp_msg{data = #amf_cmd{name = Name} = Cmd} = Msg)
  when Name == <<"receiveAudio">>; Name == <<"receiveVideo">> ->
    case Cmd#amf_cmd.body of
	[_, Flag] when is_boolean(Flag) ->
	    StreamID = Msg#rtmp_msg.stream,
	    case ?DICT:find(StreamID, Streams) of
		{ok, {Mod, StreamRef, _Type}} ->
		    Fun = to_atom(Name),
		    Ctx = make_context(State, StreamID),
		    Mod:Fun(Ctx, StreamRef, Flag);
		_ ->
		    ok
	    end,
	    {ok, State};
	_ ->
	    stop
    end;
process_amf(#state{connection_mod = Mod} = State,
	    #rtmp_msg{data = #amf_cmd{name = Method} = Cmd,
		      stream = StreamID} = Msg) ->
    case catch to_existing_atom(Method) of
	Function when is_atom(Function) ->
	    Ctx = make_context(State, StreamID),
	    [_ | Args] = Cmd#amf_cmd.body,
	    case apply(Mod, Function, [Ctx | Args]) of
		{Type, ResAMF} when Type == result;
				    Type == error;
				    Type == status ->
		    ResCmd = Cmd#amf_cmd{name = Type,
					 body = [null, ResAMF]},
		    rtmp_socket:send(State#state.rtmp_sock,
				     Msg#rtmp_msg{timestamp = 0,
						  data = ResCmd}),
		    {ok, State};
		pass ->
		    {ok, State};
		Bad ->
		    erlang:error({bad_api_return, Bad})
	    end;
	_ ->
	    stop
    end.

process_so(#state{app_name = AppName} = State,
	   #rtmp_msg{data = SO}) ->
    case oms_so_mgr:process(AppName, self(), SO) of
	{add, Name} ->
	    NewSOs = ?SETS:add_element(Name, State#state.sos),
	    {ok, State#state{sos = NewSOs}};
	{del, Name} ->
	    NewSOs = ?SETS:del_element(Name, State#state.sos),
	    {ok, State#state{sos = NewSOs}};
	ok ->
	    {ok, State};
	_Err ->
	    stop
    end.

%%--------------------------------------------------------------------
%% Aux functions
%%--------------------------------------------------------------------
info(Code) ->
    info(Code, []).

info(Code, Args) ->
    [{"level", info_level(Code)},
     {"code", Code} | Args].

to_string(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_string(Str) when is_list(Str) ->
    Str;
to_string(_) ->
    "".

to_atom(Bin) when is_binary(Bin) ->
    to_atom(binary_to_list(Bin));
to_atom(Str) when is_list(Str) ->
    list_to_atom(Str).

to_existing_atom(Bin) when is_binary(Bin) ->
    to_existing_atom(binary_to_list(Bin));
to_existing_atom(Str) when is_list(Str) ->
    list_to_existing_atom(Str).

make_context(State) ->
    make_context(State, 0).

make_context(#state{rtmp_sock = RTMPSock, addr = Addr,
		    app_name = AppName, app_options = Opts},
	     Stream) ->
    #call_ctx{rtmp_sock = RTMPSock,
	      session = self(),
	      app_name = AppName,
	      stream = Stream,
	      addr = Addr,
	      options = Opts}.

call(Mod, Fun, Args) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
	true ->
	    apply(Mod, Fun, Args);
	false ->
	    case Fun of
		connect ->
		    ok;
		play ->
		    [_, Name] = Args,
		    {play, to_string(Name)};
		publish ->
		    [_, Name, _Type] = Args,
		    {publish, to_string(Name)};
		_ ->
		    pass
	    end
    end.

get_publish_args([]) ->
    ["", ""];
get_publish_args([Name]) ->
    [to_string(Name), ""];
get_publish_args([Name, Type]) ->
    [to_string(Name), to_string(Type)].

info_level(?NC_CALL_FAILED) -> "error";
info_level(?NC_CONNECT_APP_SHUTDOWN) -> "error";
info_level(?NC_CONNECT_CLOSED) -> "status";
info_level(?NC_CONNECT_FAILED) -> "error";
info_level(?NC_CONNECT_REJECTED) -> "error";
info_level(?NC_CONNECT_SUCCESS) -> "status";
info_level(?NS_BUFFER_EMPTY) -> "status";
info_level(?NS_BUFFER_FULL) -> "status";
info_level(?NS_FAILED) -> "error";
info_level(?NS_PAUSE_NOTIFY) -> "status";
info_level(?NS_PLAY_COMPLETE) -> "status";
info_level(?NS_PLAY_FAILED) -> "error";
info_level(?NS_PLAY_INSUFFICIENT_BW) -> "warning";
info_level(?NS_PLAY_PUBLISH_NOTIFY) -> "status";
info_level(?NS_PLAY_RESET) -> "status";
info_level(?NS_PLAY_START) -> "status";
info_level(?NS_PLAY_STOP) -> "status";
info_level(?NS_PLAY_STREAM_NOT_FOUND) -> "error";
info_level(?NS_PLAY_SWITCH) -> "status";
info_level(?NS_PLAY_UNPUBLISH_NOTIFY) -> "status";
info_level(?NS_PUBLISH_BAD_NAME) -> "error";
info_level(?NS_PUBLISH_IDLE) -> "status";
info_level(?NS_PUBLISH_START) -> "status";
info_level(?NS_RECORD_FAILED) -> "error";
info_level(?NS_RECORD_NO_ACCESS) -> "error";
info_level(?NS_RECORD_START) -> "status";
info_level(?NS_RECORD_STOP) -> "status";
info_level(?NS_SEEK_FAILED) -> "error";
info_level(?NS_SEEK_NOTIFY) -> "status";
info_level(?NS_UNPAUSE_NOTIFY) -> "status";
info_level(?NS_UNPUBLISH_SUCCESS) -> "status".
