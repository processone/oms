%%%-------------------------------------------------------------------
%%% File    : amf0_codec.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 10 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(amf0_codec).

%% API
-export([decode/1, decode_shared_objects/1,
	 encode/1, encode_shared_objects/1]).

-define(NUMBER,       16#00).
-define(BOOLEAN,      16#01).
-define(STRING,       16#02).
-define(OBJECT,       16#03).
-define(MOVIECLIP,    16#04).
-define(NULL,         16#05).
-define(UNDEFINED,    16#06).
-define(REFERENCE,    16#07).
-define(ECMA_ARRAY,   16#08).
-define(OBJECT_END,   16#09).
-define(STRICT_ARRAY, 16#0a).
-define(DATE,         16#0b).
-define(LONG_STRING,  16#0c).
-define(UNSUPPORTED,  16#0d).
-define(RECORDSET,    15#0e).
-define(XML,          16#0f).
-define(TYPED_OBJECT, 16#10).
-define(AVMPLUS,      16#11).

%%====================================================================
%% API
%%====================================================================
decode(Data) ->
    decode(Data, []).

decode_shared_objects(Data) ->
    dec_shared_objects(Data, []).

encode(AMF) ->
    encode(AMF, []).

encode_shared_objects(ObjList) ->
    enc_shared_objects(ObjList, []).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Decoding
%%--------------------------------------------------------------------
decode(Data, Acc) ->
    {Result, Rest} = dec(Data),
    if Rest == <<>> ->
	    lists:reverse([Result | Acc]);
       true ->
	    decode(Rest, [Result | Acc])
    end.

dec(<<?NUMBER, Num:64/float, Rest/binary>>) ->
    {Num, Rest};
dec(<<?BOOLEAN, 0, Rest/binary>>) ->
    {false, Rest};
dec(<<?BOOLEAN, _, Rest/binary>>) ->
    {true, Rest};
dec(<<?STRING, Size:16, String:Size/binary, Rest/binary>>) ->
    {String, Rest};
dec(<<?MOVIECLIP, Rest/binary>>) ->
    {movie_clip, Rest};
dec(<<?NULL, Rest/binary>>) ->
    {null, Rest};
dec(<<?UNDEFINED, Rest/binary>>) ->
    {undefined, Rest};
dec(<<?REFERENCE, Ref:16, Rest/binary>>) ->
    {{ref, Ref}, Rest};
dec(<<?LONG_STRING, Size:32, String:Size/binary, Rest/binary>>) ->
    {String, Rest};
dec(<<?DATE, Time:64/float, _TZ:16, Rest/binary>>) ->
    {{date, Time}, Rest};
dec(<<?UNSUPPORTED, Rest/binary>>) ->
    {unsupported, Rest};
dec(<<?XML, Size:32, XML:Size/binary, Rest/binary>>) ->
    {{xml, XML}, Rest};
dec(<<?STRICT_ARRAY, N:32, Data/binary>>) ->
    dec_strict_array(Data, N, []);
dec(<<?ECMA_ARRAY, _N:32, Data/binary>>) ->
    {Obj, Rest} = dec_object(Data, []),
    {{ecma_array, Obj}, Rest};
dec(<<?OBJECT, Data/binary>>) ->
    {Obj, Rest} = dec_object(Data, []),
    {{object, Obj}, Rest};
dec(<<?TYPED_OBJECT, Size:16, Name:Size/binary, Rest/binary>>) ->
    {Obj, Rest} = dec_object(Rest, []),
    {{object, Name, Obj}, Rest}.

dec_strict_array(Rest, 0, Acc) ->
    {{array, lists:reverse(Acc)}, Rest};
dec_strict_array(Data, N, Acc) ->
    {Result, Rest} = dec(Data),
    dec_strict_array(Rest, N-1, [Result | Acc]).

dec_object(<<0:16, ?OBJECT_END, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
dec_object(<<Size:16, String:Size/binary, Data/binary>>, Acc) ->
    {Result, Rest} = dec(Data),
    dec_object(Rest, [{String, Result} | Acc]).

dec_shared_objects(<<Type, Size:32, Data:Size/binary, Rest/binary>>, Acc) ->
    KeyVals = dec_keyvals(Data, []),
    dec_shared_objects(Rest, [{Type, KeyVals}|Acc]);
dec_shared_objects(<<>>, Acc) ->
    lists:reverse(Acc).

dec_keyvals(<<Size:16, String:Size/binary, Data/binary>>, Acc) ->
    {Result, Rest} = dec(Data),
    dec_keyvals(Rest, [{String, Result} | Acc]);
dec_keyvals(<<>>, Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% Encoding
%%--------------------------------------------------------------------
encode([Val | Tail], Acc) ->
    encode(Tail, [enc(Val) | Acc]);
encode([], Acc) ->
    iolist_to_binary(lists:reverse(Acc)).

enc(Num) when is_number(Num) ->
    <<?NUMBER, Num:64/float>>;
enc(false) ->
    <<?BOOLEAN, 0>>;
enc(true) ->
    <<?BOOLEAN, 1>>;
enc(Str) when is_binary(Str); is_list(Str) ->
    Data = iolist_to_binary(Str),
    Size = size(Data),
    if Size =< 16#ffff ->
	    <<?STRING, Size:16, Data/binary>>;
       true ->
	    <<?STRING, Size:32, Data/binary>>
   end;
enc(movie_clip) ->
    ?MOVIECLIP;
enc(null) ->
    ?NULL;
enc(undefined) ->
    ?UNDEFINED;
enc({data, Time}) ->
    <<?DATE, Time:64/float, 0:16>>;
enc(unsupported) ->
    ?UNSUPPORTED;
enc({xml, Str}) ->
    Data = iolist_to_binary(Str),
    <<?XML, (size(Data)):32, Data/binary>>;
enc({object, Obj}) ->
    enc_object(Obj, [<<?OBJECT>>]);
enc({object, Name, Obj}) ->
    Name1 = iolist_to_binary(Name),
    enc_object(Obj, [<<?TYPED_OBJECT, (size(Name)):16, Name1/binary>>]);
enc({ecma_array, Obj}) ->
    enc_object(Obj, [<<?ECMA_ARRAY, (length(Obj)):32>>]);
enc({array, Array}) ->
    enc_strict_array(Array, [<<?STRICT_ARRAY, (length(Array)):32>>]).

enc_object([{Type, Val} | Tail], Acc) ->
    Key = iolist_to_binary(Type),
    Data = <<(size(Key)):16, Key/binary>>,
    enc_object(Tail, [[Data, enc(Val)] | Acc]);
enc_object([], Acc) ->
    lists:reverse([<<0:16, ?OBJECT_END>> | Acc]).

enc_strict_array([Val | Tail], Acc) ->
    enc_strict_array(Tail, [enc(Val) | Acc]);
enc_strict_array([], Acc) ->
    lists:reverse(Acc).

enc_shared_objects([{EventType, KeyVals}|Tail], Acc) ->
    EncKeyVals = enc_keyvals(KeyVals, []),
    Data = <<EventType, (size(EncKeyVals)):32, EncKeyVals/binary>>,
    enc_shared_objects(Tail, [Data, Acc]);
enc_shared_objects([], Acc) ->
    iolist_to_binary(lists:reverse(Acc)).

enc_keyvals([{Str, Val} | Tail], Acc) ->
    Name = iolist_to_binary(Str),
    Data = <<(size(Name)):16, Name/binary>>,
    enc_keyvals(Tail, [[Data, enc(Val)] | Acc]);
enc_keyvals([], Acc) ->
    iolist_to_binary(lists:reverse(Acc)).
