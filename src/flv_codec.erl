%%%-------------------------------------------------------------------
%%% File    : flv_codec.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 26 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(flv_codec).

%% API
%%-compile(export_all).
-export([open/1, open/2, close/1, read/1,
	 read/2, position/2, write/4, format_error/1]).

-record(state, {fd,
		type,
		video_codec_id = 0,
		audio_codec_id = 0,
		position = 0,
		timestamp = 0,
		filesize = 0}).

-include("rtmp.hrl").

-define(V1, 1).
-define(V1_HEADER_SIZE, 9).
-define(READ_WRITE_BUFSIZE, 65536).

-define(is_valid_tag(Type), 
	((Type == ?AUDIO) or (Type == ?VIDEO)
	 or (Type == ?AMF0_DATA))).

%%====================================================================
%% API
%%====================================================================
open(FileName) ->
    open(FileName, []).

open(FileName, Opts) ->
    FileOpts = [Type|_] =
	case lists:member(write, Opts) of
	    true ->
		[write, binary, raw];
	    false ->
		[read, binary, raw, read_ahead]
	end,
    case file:open(FileName, FileOpts) of
	{ok, Fd} ->
	    State = #state{fd = Fd, type = Type},
	    if Type == write ->
		    {ok, State};
	       true ->
		    case read_header(Fd) of
			{ok, Pos} ->
			    FileSize = filelib:file_size(FileName),
			    {ok, State#state{position = Pos,
					     filesize = FileSize}};
			Err ->
			    Err
		    end
	    end;
	Err ->
	    Err
    end.

close(#state{type = read, fd = Fd}) ->
    file:close(Fd);
close(#state{filesize = 0, fd = Fd}) ->
    file:close(Fd);
close(#state{fd = Fd} = State) ->
    Meta = make_meta_tag(State),
    file:pwrite(Fd, 13, Meta),
    file:close(Fd).

read(#state{fd = Fd, type = read} = State) ->
    case file:read(Fd, 11) of
	{ok, <<TagType, DataSize:24, TimeStamp:24, ExtTimeStamp, 0:24>>} ->
	    if ?is_valid_tag(TagType) ->
		    <<NewTimeStamp:32>> = <<ExtTimeStamp, TimeStamp:24>>,
		    TagSize = DataSize+11,
		    case file:read(Fd, DataSize+4) of
			{ok, <<Data:DataSize/binary, _TagSize:32>>} ->
			    case decode_data(TagType, Data) of
				{ok, Payload} ->
				    PrevPosition = State#state.position,
				    NewPosition = PrevPosition + TagSize + 4,
				    {ok, {TagType, NewTimeStamp, Payload},
				     State#state{timestamp = NewTimeStamp,
						 position = NewPosition}};
				Err ->
				    Err
			    end;
			{ok, _} ->
			    {error, bad_format};
			Err ->
			    Err
		    end;
	       true ->
		    {error, bad_format}
	    end;
	{ok, _} ->
	    eof;
	Err ->
	    Err
    end;
read(_State) ->
    {error, badarg}.

read(#state{type = read} = State, TimeStamp) ->
    case find_closest_tag(State, TimeStamp) of
	{ok, NewState} ->
	    read(NewState);
	Err ->
	    Err
    end;
read(_, _) ->
    {error, badarg}.

position(#state{type = read} = State, TimeStamp) ->
    case find_closest_tag(State, TimeStamp) of
	{ok, NewState} ->
	    {ok, NewState};
	Err ->
	    Err
    end;
position(_, _) ->
    {error, badarg}.

write(#state{type = write,
	     filesize = FileSize,
	     fd = Fd} = State,
      Type, TimeStamp, Data) when Type == ?AUDIO; Type == ?VIDEO ->
    NewData = encode_data(Type, Data),
    <<ExtTimeStamp, NewTimeStamp:24>> = <<TimeStamp:32>>,
    Tag = <<Type, (size(NewData)):24, NewTimeStamp:24,
	   ExtTimeStamp, 0:24, NewData/binary>>,
    TagSize = size(Tag),
    Res = if FileSize == 0 ->
		  Header = <<"FLV", ?V1, 5, ?V1_HEADER_SIZE:32, 0:32>>,
		  Meta = make_meta_tag(State),
		  <<Header/binary, Meta/binary, Tag/binary, TagSize:32>>;
	     true ->
		  <<Tag/binary, TagSize:32>>
          end,
    case file:write(Fd, Res) of
	ok ->
	    NewState = update_av_info(Type, Data, State),
	    {ok, NewState#state{filesize = FileSize + size(Res),
				timestamp = TimeStamp}};
	Err ->
	    Err
    end;
write(#state{type = write} = State, _, _, _) ->
    {ok, State};
write(_, _, _, _) ->
    {error, badarg}.

format_error(bad_format) ->
    "bad file format";
format_error(Reason) ->
    file:format_error(Reason).

%%====================================================================
%% Internal functions
%%====================================================================
read_header(Fd) ->
    Position = ?V1_HEADER_SIZE + 4,
    case file:read(Fd, Position) of
	{ok, <<"FLV", ?V1, _Flags, ?V1_HEADER_SIZE:32, 0:32>>} ->
	    {ok, Position};
	{ok, _} ->
	    {error, bad_format};
	Err ->
	    Err
    end.

decode_data(?AUDIO, Data) ->
    {ok, Data};
decode_data(?VIDEO, Data) ->
    {ok, Data};
decode_data(?AMF0_DATA, Data) ->
    {ok, amf0_codec:decode(Data)};
decode_data(_Type, _Data) ->
    {error, bad_format}.

encode_data(?AUDIO, Data) ->
    Data;
encode_data(?VIDEO, Data) ->
    Data;
encode_data(?AMF0_DATA, AMF) ->
    amf0_codec:encode(AMF).

%% TODO: improve meta data detection
update_av_info(?AUDIO, <<C, _/binary>>, State) ->
    ACID = (C band 16#f0) bsr 4,
    State#state{audio_codec_id = ACID};
update_av_info(?VIDEO, <<C, _/binary>>, State) ->
    VCID = (C band 16#0f) bsr 0,
    State#state{video_codec_id = VCID};
update_av_info(_, _, State) ->
    State.

make_meta_tag(#state{video_codec_id = VCID,
		     audio_codec_id = ACID,
		     filesize = FileSize,
		     timestamp = TimeStamp}) ->
    Duration = TimeStamp / 1000,
    OnMeta = ["onMetaData",
	      {ecma_array, [{"duration", Duration},
			    {"starttime", 0},
			    {"width", 320},
			    {"height", 240},
			    {"videocodecid", VCID},
			    {"audiocodecid", ACID},
			    {"filesize", FileSize},
			    {"canseekontime", true},
			    {"canseekonend", true}]}],
    Data = amf0_codec:encode(OnMeta),
    Tag = <<?AMF0_DATA, (size(Data)):24, 0:56, Data/binary>>,
    <<Tag/binary, (size(Tag)):32>>.

%%====================================================================
%% Seeking functions
%%
%% We are using quite obfuscated heuristic algorithm
%% to find the FLV tag. To find the needed timestamp
%% we are using binary search between the tags.
%% A complexity of the algorithm is O(log(N)), which
%% is much better than linear search with O(N),
%% especially on big files (~100Mb and above) where
%% linear search may takes several seconds to find the tag.
%%====================================================================
find_closest_tag(#state{timestamp = TimeStamp} = State, TimeStamp) ->
    {ok, State};
find_closest_tag(State, TimeStamp) when TimeStamp =< 0 ->
    Pos = ?V1_HEADER_SIZE + 4,
    file:position(State#state.fd, Pos),
    {ok, State#state{position = Pos}};
find_closest_tag(#state{fd = Fd,
			filesize = FileSize,
			position = CurrentPosition} = State,
		 SeekTimeStamp) ->
    Pos = trunc(FileSize/2),
    case find_tag(Fd, {0, Pos, FileSize}, undefined, SeekTimeStamp) of
	{ok, FoundPosition} ->
	    file:position(Fd, FoundPosition),
	    {ok, State#state{position = FoundPosition}};
	eof ->
	    file:position(Fd, FileSize - 1),
	    {ok, State#state{position = FileSize - 1}};
	not_found ->
	    file:position(Fd, CurrentPosition),
	    {ok, State};
	Err ->
	    file:position(Fd, CurrentPosition),
	    Err
    end.

find_tag(_Fd, {LeftPos, _SeekPos, RightPos}, Delta, _SeekTS)
  when abs(RightPos - LeftPos) >= Delta ->
    {ok, RightPos};
find_tag(Fd, {LeftPos, SeekPos, RightPos}, _Delta, SeekTS) ->
    case find_tag_heuristic(Fd, SeekPos) of
	{ok, NewPos, SeekTS} ->
	    {ok, NewPos};
	{ok, NewPos, NewTS} ->
	    {NewLeftPos, NewRightPos} = 
		if SeekTS >= NewTS ->
			{NewPos, RightPos};
		   SeekTS < NewTS ->
			{LeftPos, NewPos}
		end,
	    Pos = trunc((NewLeftPos + NewRightPos)/2),
	    NewDelta = abs(LeftPos - RightPos),
	    find_tag(Fd, {NewLeftPos, Pos, NewRightPos}, NewDelta, SeekTS);
	Err ->
	    Err
    end.

find_tag_heuristic(_Fd, Pos) when Pos =< 0 ->
    {ok, ?V1_HEADER_SIZE + 4, 0};
find_tag_heuristic(Fd, Pos) ->
    find_tag_heuristic(Fd, Pos, 10240).

find_tag_heuristic(_Fd, _Pos, BufLen) when BufLen > 262144 ->
    not_found;
find_tag_heuristic(Fd, Pos, BufLen) ->
    case file:pread(Fd, Pos, BufLen) of
	{ok, Buf} when size(Buf) == BufLen ->
	    case find_tag_in_buf(Buf, 0) of
		{ok, RelativePos, TimeStamp} ->
		    {ok, Pos + RelativePos, TimeStamp};
		not_found ->
		    find_tag_heuristic(Fd, Pos+BufLen, BufLen*4)
	    end;
	{error, _} = Err ->
	    Err;
	_ ->
	    eof
    end.

find_tag_in_buf(<<Type, Size:24, TimeStamp:24, ExtTimeStamp,
		 0:24, _:Size/binary, Rest:4/binary, _/binary>> = Data, Pos)
  when ?is_valid_tag(Type) ->
    %% WORKAROUND: some buggy encoders write tag size in little-endian
    case Rest of
	<<TagSize:32>> when TagSize == Size + 11 ->
	    <<NewTimeStamp:32>> = <<ExtTimeStamp, TimeStamp:24>>,
	    {ok, Pos, NewTimeStamp};
	<<TagSize:32/little>> when TagSize == Size + 11 ->
	    <<NewTimeStamp:32>> = <<ExtTimeStamp, TimeStamp:24>>,
	    {ok, Pos, NewTimeStamp};
	_ ->
	    <<_, Tail/binary>> = Data,
	    find_tag_in_buf(Tail, Pos+1)
    end;
find_tag_in_buf(<<_, Tail/binary>>, Pos) ->
    find_tag_in_buf(Tail, Pos+1);
find_tag_in_buf(_, _) ->
    not_found.
