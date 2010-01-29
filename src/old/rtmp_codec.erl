%%%-------------------------------------------------------------------
%%% File    : rtmp_codec.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 27 Nov 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(rtmp_codec).

%% API
-export([new/0,
	 decode/2,
	 encode/2,
	 set_chunk_size/3,
	 get_chunk_size/2,
	 abort/2,
	 pp/1]).

-include("rtmp.hrl").
-include("p1_logger.hrl").

-define(CHUNK_SIZE, 128).
-define(DICT, dict).
-define(MAX_TIMESTAMP, 16#00ffffff).

%% Refer to Adobe's RTMP specification for details
%% about RTMP stream packet structure
-record(rtmp, {chunk_type,      %% Chunk Type
	       chunk_stream_id, %% Chunk Stream ID
	       timestamp,       %% TimeStamp or TimeStampDelta
	       msg_type_id,     %% RTMP Message Type ID
	       msg_stream_id    %% RTMP Message Stream ID
	      }).

-record(chunk, {type,
		stream,
		len,
		completed,
		timestamp,
		timestamp_delta,
		data}).

-record(state, {chunk_size_in = ?CHUNK_SIZE,
		chunk_size_out = ?CHUNK_SIZE,
		chunk_buf_in = ?DICT:new(),
		chunk_buf_out = ?DICT:new()}).

%%====================================================================
%% API
%%====================================================================
new() ->
    #state{}.

set_chunk_size(Direction, Size, State)
  when is_integer(Size), Size >= ?CHUNK_SIZE ->
    case Direction of
	in ->
	    State#state{chunk_size_in = Size};
	out ->
	    State#state{chunk_size_out = Size}
    end.

get_chunk_size(Direction, State) ->
    case Direction of
	in ->
	    State#state.chunk_size_in;
	out ->
	    State#state.chunk_size_out
    end.

abort(CSID, State) ->
    ChunkBuf = ?DICT:erase(CSID, State#state.chunk_buf_in),
    State#state{chunk_buf_in = ChunkBuf}.

decode(Data, State) ->
    decode_rtmp(Data, State).

encode(#rtmp_msg{type = Type,
		 stream = Stream,
		 timestamp = TimeStamp,
		 chunk_stream = CSID,
		 data = Data},
       #state{chunk_size_out = ChunkSize,
	      chunk_buf_out = ChunkBuf} = State) ->
    Len = size(Data),
    RTMP = #rtmp{chunk_type = 0,
		 chunk_stream_id = CSID,
		 timestamp = TimeStamp,
		 msg_type_id = Type,
		 msg_stream_id = Stream},
    NewRTMP = case ?DICT:find(CSID, ChunkBuf) of
		  {ok, #chunk{stream = PrevStream,
			      type = PrevType,
			      len = PrevLen,
			      timestamp = PrevTimeStamp}} ->
		      Delta = TimeStamp - PrevTimeStamp,
		      if PrevStream /= Stream ->
			      RTMP;
			 TimeStamp < PrevTimeStamp ->
			      RTMP;
			 Type /= PrevType ->
			      RTMP#rtmp{chunk_type = 1,
					timestamp = Delta};
			 Len /= PrevLen ->
			      RTMP#rtmp{chunk_type = 1,
					timestamp = Delta};
			 Len > ChunkSize ->
			      RTMP#rtmp{chunk_type = 1,
					timestamp = Delta};
			 TimeStamp /= PrevTimeStamp ->
			      RTMP#rtmp{chunk_type = 2,
					timestamp = Delta};
			 true ->
			      RTMP#rtmp{chunk_type = 3}
		      end;
		  error ->
		      RTMP
	      end,
    NewChunkBuf = ?DICT:store(CSID, #chunk{stream = Stream,
					   type = Type,
					   len = Len,
					   timestamp = TimeStamp},
			      ChunkBuf),
    NewState = State#state{chunk_buf_out = NewChunkBuf},
    encode(NewRTMP, Len, Data, NewState).

pp(RTMP) ->
    lists:flatten(io_lib_pretty:print(RTMP, fun pp/2)).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Decoding
%%--------------------------------------------------------------------
decode_rtmp(<<Fmt:2, 0:6, CSID, Rest/binary>>, State) ->
    decode_rtmp_header(#rtmp{chunk_type = Fmt,
			     chunk_stream_id = CSID + 64},
		       State, Rest);
decode_rtmp(<<Fmt:2, 1:6, CSID:16, Rest/binary>>, State) ->
    decode_rtmp_header(#rtmp{chunk_type = Fmt,
			     chunk_stream_id = CSID + 64},
		       State, Rest);
decode_rtmp(<<Fmt:2, CSID:6, Rest/binary>>, State) ->
    decode_rtmp_header(#rtmp{chunk_type = Fmt,
			     chunk_stream_id = CSID},
		       State, Rest);
decode_rtmp(_Data, _State) ->
    more.

decode_rtmp_header(#rtmp{chunk_type = 0} = RTMP, State,
		   <<TimeStamp:24, MsgLen:24, MsgTypeID,
		    MsgStreamID:32/little, Rest/binary>>) ->
    decode_rtmp_ext_ts(
      RTMP#rtmp{timestamp = TimeStamp,
		msg_type_id = MsgTypeID,
		msg_stream_id = MsgStreamID},
      MsgLen, State, Rest);
decode_rtmp_header(#rtmp{chunk_type = 1} = RTMP, State,
		   <<TimeStampDelta:24, MsgLen:24,
		    MsgTypeID, Rest/binary>>) ->
    decode_rtmp_ext_ts(
      RTMP#rtmp{timestamp = TimeStampDelta,
		msg_type_id = MsgTypeID},
      MsgLen, State, Rest);
decode_rtmp_header(#rtmp{chunk_type = 2} = RTMP, State,
		   <<TimeStampDelta:24, Rest/binary>>) ->
    decode_rtmp_ext_ts(
      RTMP#rtmp{timestamp = TimeStampDelta}, 0, State, Rest);
decode_rtmp_header(#rtmp{chunk_type = 3} = RTMP, State, Data) ->
    decode_rtmp_data(RTMP, 0, State, Data);
decode_rtmp_header(_RTMP, _State, _Data) ->
    more.

decode_rtmp_ext_ts(#rtmp{timestamp = ?MAX_TIMESTAMP} = RTMP,
		   MsgLen, State, <<ExtTimeStamp:32, Rest/binary>>) ->
    NewTimeStamp = if ExtTimeStamp == 0 ->
			   ?MAX_TIMESTAMP;
		      true ->
			   ExtTimeStamp
		   end,
    decode_rtmp_data(RTMP#rtmp{timestamp = NewTimeStamp},
		     MsgLen, State, Rest);
decode_rtmp_ext_ts(#rtmp{timestamp = ?MAX_TIMESTAMP},
		   _MsgLen, _State, _Data) ->
    more;
decode_rtmp_ext_ts(RTMP, MsgLen, State, Data) ->
    decode_rtmp_data(RTMP, MsgLen, State, Data).

decode_rtmp_data(#rtmp{chunk_type = 0,
		       chunk_stream_id = CSID,
		       timestamp = TimeStamp,
		       msg_type_id = MsgTypeID,
		       msg_stream_id = MsgStreamID},
		 MsgLen, State, Bin) ->
    ChunkSize = State#state.chunk_size_in,
    Len = lists:min([MsgLen, ChunkSize]),
    Chunk = #chunk{type = MsgTypeID,
		   stream = MsgStreamID,
		   len = MsgLen,
		   timestamp = TimeStamp,
		   timestamp_delta = 0,
		   completed = ChunkSize >= MsgLen},
    make_rtmp_msg(Chunk, State, CSID, Len, Bin);
decode_rtmp_data(#rtmp{chunk_type = 1,
		       chunk_stream_id = CSID,
		       timestamp = TimeStampDelta,
		       msg_type_id = MsgTypeID},
		 MsgLen, State, Bin) ->
    ChunkSize = State#state.chunk_size_in,
    #chunk{timestamp = TimeStamp,
	   stream = MsgStreamID} =
	?DICT:fetch(CSID, State#state.chunk_buf_in),
    Len = lists:min([MsgLen, ChunkSize]),
    NewTimeStamp = TimeStamp + TimeStampDelta,
    Chunk = #chunk{type = MsgTypeID,
		   stream = MsgStreamID,
		   len = MsgLen,
		   completed = ChunkSize >= MsgLen,
		   timestamp = NewTimeStamp,
		   timestamp_delta = TimeStampDelta},
    make_rtmp_msg(Chunk, State, CSID, Len, Bin);
decode_rtmp_data(#rtmp{chunk_type = 2,
		       chunk_stream_id = CSID,
		       timestamp = TimeStampDelta},
		 _, State, Bin) ->
    ChunkSize = State#state.chunk_size_in,
    #chunk{timestamp = TimeStamp,
	   type = MsgTypeID,
	   stream = MsgStreamID,
	   len = MsgLen} =
	?DICT:fetch(CSID, State#state.chunk_buf_in),
    Len = lists:min([MsgLen, ChunkSize]),
    NewTimeStamp = TimeStamp + TimeStampDelta,
    Chunk = #chunk{type = MsgTypeID,
		   stream = MsgStreamID,
		   len = MsgLen,
		   completed = ChunkSize >= MsgLen,
		   timestamp = NewTimeStamp,
		   timestamp_delta = TimeStampDelta},
    make_rtmp_msg(Chunk, State, CSID, Len, Bin);
decode_rtmp_data(#rtmp{chunk_type = 3,
		       chunk_stream_id = CSID},
		 _, State, Bin) ->
    ChunkSize = State#state.chunk_size_in,
    #chunk{timestamp = TimeStamp,
	   type = MsgTypeID,
	   stream = MsgStreamID,
	   completed = IsCompleted,
	   timestamp_delta = TimeStampDelta,
	   data = BufData,
	   len = MsgLen} =
	?DICT:fetch(CSID, State#state.chunk_buf_in),
    if IsCompleted ->
	    Len = lists:min([MsgLen, ChunkSize]),
	    NewTimeStamp = TimeStamp + TimeStampDelta,
	    Chunk = #chunk{type = MsgTypeID,
			   stream = MsgStreamID,
			   len = MsgLen,
			   completed = ChunkSize >= MsgLen,
			   timestamp = NewTimeStamp,
			   timestamp_delta = TimeStampDelta},
	    make_rtmp_msg(Chunk, State, CSID, Len, Bin);
       true ->
	    Len = lists:min([MsgLen - size(BufData), ChunkSize]),
	    Chunk = #chunk{type = MsgTypeID,
			   stream = MsgStreamID,
			   len = MsgLen,
			   completed = (Len + size(BufData)) >= MsgLen,
			   timestamp = TimeStamp,
			   timestamp_delta = TimeStampDelta},
	    make_rtmp_msg(Chunk, State, CSID,
			  Len + size(BufData),
			  <<BufData/binary, Bin/binary>>)
    end.

make_rtmp_msg(#chunk{type = Type,
		     stream = Stream,
		     completed = IsCompleted,
		     timestamp = TimeStamp} = Chunk,
	      State, CSID, Len, Bin) ->
    case Bin of
	<<Data:Len/binary, Rest/binary>> ->
	    NewChunk = if IsCompleted ->
			       Chunk#chunk{data = <<>>};
			  true ->
			       Chunk#chunk{data = Data}
		       end,
	    ChunkBuf = ?DICT:store(
			  CSID, NewChunk,
			  State#state.chunk_buf_in),
	    NewState = State#state{chunk_buf_in = ChunkBuf},
	    if IsCompleted ->
		    Msg = #rtmp_msg{type = Type,
				    stream = Stream,
				    timestamp = TimeStamp,
				    chunk_stream = CSID,
				    data = Data},
		    {ok, Msg, NewState, Rest};
	       true ->
		    {more, NewState, Rest}
	    end;
	_ ->
	    more
    end.

%%--------------------------------------------------------------------
%% Encoding
%%--------------------------------------------------------------------
encode(RTMP, Len, Data, #state{chunk_size_out = ChunkSize} = State)
  when Len > ChunkSize ->
    [Head|Tail] = split_data(Data, ChunkSize),
    Res = [encode_rtmp(RTMP, Len, Head) |
	   [encode_rtmp(RTMP#rtmp{chunk_type = 3}, undefined, Rest)
	    || Rest <- Tail]],
    {Res, State};
encode(RTMP, Len, Data, State) ->
    {encode_rtmp(RTMP, Len, Data), State}.

encode_rtmp(#rtmp{chunk_type = ChunkType,
		  chunk_stream_id = CSID,
		  timestamp = TimeStamp,
		  msg_type_id = MsgTypeID,
		  msg_stream_id = MsgStreamID},
	    ChunkLen, Data) ->
    [encode_rtmp_basic_header(ChunkType, CSID),
     encode_rtmp_ts(ChunkType, TimeStamp),
     encode_rtmp_len(ChunkType, ChunkLen),
     encode_rtmp_type_id(ChunkType, MsgTypeID),
     encode_rtmp_stream_id(ChunkType, MsgStreamID),
     encode_rtmp_ext_ts(ChunkType, TimeStamp),
     Data].

encode_rtmp_basic_header(Type, ID) ->
    if ID < 64 ->
	    <<Type:2, ID:6>>;
       ID < 320 ->
	    <<Type:2, 0:6, (ID - 64)>>;
       true ->
	    <<Type:2, 1:6, (ID - 64):16>>
    end.

encode_rtmp_ts(Type, TimeStamp) when Type < 3 ->
    <<(TimeStamp band ?MAX_TIMESTAMP):24>>;
encode_rtmp_ts(_, _) ->
    <<>>.

encode_rtmp_ext_ts(Type, TimeStamp) when Type < 3 ->
    if TimeStamp > ?MAX_TIMESTAMP ->
	    <<TimeStamp:32>>;
       TimeStamp == ?MAX_TIMESTAMP ->
	    <<0:32>>;
       is_integer(TimeStamp) ->
	    <<>>
    end;
encode_rtmp_ext_ts(_, _) ->
    <<>>.

encode_rtmp_len(Type, Int) when Type < 2 ->
    <<Int:24>>;
encode_rtmp_len(_, _) ->
    <<>>.

encode_rtmp_type_id(Type, Int) when Type < 2 ->
    Int;
encode_rtmp_type_id(_, _) ->
    <<>>.

encode_rtmp_stream_id(0, Int) ->
    <<Int:32/little>>;
encode_rtmp_stream_id(_, _) ->
    <<>>.

%%--------------------------------------------------------------------
%% Aux functions
%%--------------------------------------------------------------------
split_data(<<>>, _Size) ->
    [<<>>];
split_data(Data, Size) ->
    split_data(Data, Size, []).

split_data(<<>>, _Size, Acc) ->
    lists:reverse(Acc);
split_data(Data, Size, Acc) ->
    case Data of
	<<Head:Size/binary, Tail/binary>> ->
	    split_data(Tail, Size, [Head | Acc]);
	_ ->
	    lists:reverse([Data | Acc])
    end.

%%--------------------------------------------------------------------
%% Pretty printer
%%--------------------------------------------------------------------
pp(Tag, N) ->
    try
	pp1(Tag, N)
    catch _:_ ->
	    no
    end.

pp1(rtmp_msg, N) ->
    N = record_info(size, rtmp_msg) - 1,
    record_info(fields, rtmp_msg);
pp1(chunk, N) ->
    N = record_info(size, chunk) - 1,
    record_info(fields, chunk);
pp1(rtmp, N) ->
    N = record_info(size, rtmp) - 1,
    record_info(fields, rtmp);
pp1(amf_cmd, N) ->
    N = record_info(size, amf_cmd) - 1,
    record_info(fields, amf_cmd);
pp1(_, _) ->
    no.
