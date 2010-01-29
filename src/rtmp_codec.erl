%%%-------------------------------------------------------------------
%%% File    : rtmp_codec.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 27 Nov 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(rtmp_codec).

%% API
%%-compile(export_all).

-export([new/0,
	 close/1,
	 decode/2,
	 encode/2,
	 set_chunk_size/3,
	 get_chunk_size/2,
	 alloc_csid/1,
	 abort/2]).

-include("rtmp.hrl").

-define(ENCODE, 1).
-define(DECODE, 2).
-define(OK, 3).
-define(MORE, 4).
-define(CONTINUE, 5).
-define(SET_CHUNK_SIZE_IN, 6).
-define(SET_CHUNK_SIZE_OUT, 7).
-define(GET_CHUNK_SIZE_IN, 8).
-define(GET_CHUNK_SIZE_OUT, 9).
-define(CHUNK_ABORT, 10).
-define(ALLOC_CSID, 11).

%%====================================================================
%% API
%%====================================================================
new() ->
    open_port({spawn, rtmp_codec_drv}, [binary]).

close(Port) when is_port(Port) ->
    port_close(Port).

set_chunk_size(Direction, Size, Port)
  when is_integer(Size), Size > 0, is_port(Port) ->
    case Direction of
	in ->
	    port_control(Port, ?SET_CHUNK_SIZE_IN, <<Size:32>>);
	out ->
	    port_control(Port, ?SET_CHUNK_SIZE_OUT, <<Size:32>>)
    end,
    Port.

get_chunk_size(Direction, Port) when is_port(Port) ->
    <<Size:32>> = case Direction of
		      in ->
			  port_control(Port, ?GET_CHUNK_SIZE_IN, <<>>);
		      out ->
			  port_control(Port, ?GET_CHUNK_SIZE_OUT, <<>>)
		  end,
    Size.

abort(CSID, Port) when is_port(Port) ->
    port_control(Port, ?CHUNK_ABORT, <<CSID:32>>),
    Port.

alloc_csid(Port) when is_port(Port) ->
    <<CSID:32>> = port_control(Port, ?ALLOC_CSID, <<>>),
    CSID.

decode(<<>>, _Port) ->
    more;
decode(Data, Port) ->
    case port_control(Port, ?DECODE, Data) of
	<<?OK, Type:32, Stream:32, TimeStamp:32,
	 CSID:32, L:32, MsgData:L/binary, Rest/binary>> ->
	    Msg = #rtmp_msg{type = Type,
			    stream = Stream,
			    timestamp = TimeStamp,
			    chunk_stream = CSID,
			    data = decode_payload(Type, MsgData)},
	    {ok, Msg, Port, Rest};
	<<?MORE>> ->
	    more;
	<<?CONTINUE, Rest/binary>> ->
	    {more, Port, Rest};
	<<>> ->
	    error
    end.

encode(#rtmp_msg{type = Type,
		 stream = Stream,
		 timestamp = TimeStamp,
		 chunk_stream = CSID,
		 data = Payload},
       Port) when CSID > 1 ->
    Data = encode_payload(Type, Payload),
    Pkt = <<Type:32, Stream:32, TimeStamp:32, CSID:32, Data/binary>>,
    {port_control(Port, ?ENCODE, Pkt), Port}.

%%--------------------------------------------------------------------
%% Payload decoding
%%--------------------------------------------------------------------
decode_payload(?AMF0_CMD, Data) ->
    decode_amf_command(amf0_codec:decode(Data));
decode_payload(?AMF3_CMD, <<_, Data/binary>>) ->
    decode_amf_command(amf0_codec:decode(Data));
decode_payload(?AMF0_DATA, Data) ->
    amf0_codec:decode(Data);
decode_payload(?AMF0_SO, <<Size:16, Name:Size/binary,
			  Unk:12/binary, Data/binary>>) ->
    IsPersistent = case Unk of
		       <<_:7/binary, 2, _/binary>> ->
			   true;
		       _ ->
			   false
		   end,
    Events = amf0_codec:decode_shared_objects(Data),
    #amf_so{name = Name, persistent = IsPersistent, events = Events};
decode_payload(?CHKSIZE, <<Int:32>>) ->
    Int;
decode_payload(?ABORT, <<Int:32>>) ->
    Int;
decode_payload(?ACKNO, <<Int:32>>) ->
    Int;
decode_payload(?ACKSIZE, <<Int:32>>) ->
    Int;
decode_payload(?PEERBW, <<Size:32, Limit>>) ->
    {Size, Limit};
decode_payload(?USER_CONTROL, <<Event:16, Data/binary>>) ->
    if Event == ?SET_BUFFER_LENGTH ->
	    <<StreamID:32, BufLen:32>> = Data,
	    {Event, StreamID, BufLen};
       Event < 8 ->
	    <<Int:32>> = Data,
	    {Event, Int};
       true ->
	    {Event, Data}
    end;
decode_payload(_Type, Data) ->
    Data.

decode_amf_command([Method, TrID | Rest]) ->
    #amf_cmd{name = Method, trid = TrID, body = Rest}.

%%--------------------------------------------------------------------
%% Payload encoding
%%--------------------------------------------------------------------
encode_payload(Type,
	       #amf_cmd{name = Name, trid = TrID, body = Body}) ->
    Method = case Name of
		 error -> <<"_error">>;
		 result -> <<"_result">>;
		 status -> <<"onStatus">>;
		 _ -> iolist_to_binary(Name)
	     end,
    AMF = [Method, TrID | Body],
    if Type == ?AMF0_CMD ->
	    amf0_codec:encode(AMF);
       Type == ?AMF3_CMD ->
	    <<0, (amf0_codec:encode(AMF))/binary>>
    end;
encode_payload(Type,
	       #amf_so{name = Name, persistent = IsPersistent,
		       events = Events}) ->
    NameBin = iolist_to_binary(Name),
    EncEvents = if Type == ?AMF0_SO ->
			amf0_codec:encode_shared_objects(Events);
		   Type == ?AMF3_SO ->
			amf3_codec:encode_shared_objects(Events)
		end,
    Persistent = if IsPersistent ->
			 <<0,0,0,0,0,0,0,2,0,0,0,0>>;
		    true ->
			 <<0,0,0,0,0,0,0,0,0,0,0,0>>
		 end,
    <<(size(NameBin)):16, NameBin/binary,
     Persistent/binary, EncEvents/binary>>;
encode_payload(?AMF0_DATA, AMF) when not is_binary(AMF) ->
    amf0_codec:encode(AMF);
encode_payload(?CHKSIZE, Int) ->
    <<Int:32>>;
encode_payload(?ABORT, Int) ->
    <<Int:32>>;
encode_payload(?ACKNO, Int) ->
    <<Int:32>>;
encode_payload(?ACKSIZE, Int) ->
    <<Int:32>>;
encode_payload(?PEERBW, {Size, Limit}) ->
    <<Size:32, Limit>>;
encode_payload(?USER_CONTROL, {Event, Int}) ->
    <<Event:16, Int:32>>;
encode_payload(?USER_CONTROL, {Event, StreamID, BufLen}) ->
    <<Event:16, StreamID:32, BufLen:32>>;
encode_payload(_Type, Data) ->
    Data.
