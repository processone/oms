%%%-------------------------------------------------------------------
%%% File    : oms_api.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 12 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_api).

%% API
-export([send/2,
	 sync_send/2,
	 setopts/2,
	 getopts/2,
	 alloc_csid/1,
	 close/1,
	 call/3,
	 all_sessions/1,
	 register_stream/3,
	 unregister_stream/2,
	 find_stream/2,
	 all_streams/1,
	 stream_info/3,
	 stream_info/4]).

%%====================================================================
%% API
%%====================================================================
send(RTMPSock, RTMP) ->
    rtmp_socket:send(RTMPSock, RTMP).

sync_send(RTMPSock, RTMP) ->
    rtmp_socket:sync_send(RTMPSock, RTMP).

setopts(RTMPSock, Opts) ->
    rtmp_socket:setopts(RTMPSock, Opts).

getopts(RTMPSock, OptNames) ->
    rtmp_socket:getopts(RTMPSock, OptNames).

alloc_csid(RTMPSock) ->
    rtmp_socket:alloc_csid(RTMPSock).

close(RTMPSock) ->
    rtmp_socket:close(RTMPSock).

register_stream(AppName, Name, StreamRef) when is_list(AppName) ->
    oms_sm:add_stream(AppName, Name, StreamRef).

unregister_stream(AppName, Name) when is_list(AppName) ->
    oms_sm:del_stream(AppName, Name).

find_stream(AppName, Name) when is_list(AppName) ->
    oms_sm:find_stream(AppName, Name).

all_streams(AppName) when is_list(AppName) ->
    oms_sm:all_streams(AppName).

all_sessions(AppName) when is_list(AppName) ->
    oms_sm:all_sessions(AppName).

call(Session, Fun, Args) when is_pid(Session),
			      is_atom(Fun),
			      is_list(Args) ->
    rtmp_session:amf_call(Session, Fun, Args).

stream_info(Session, Stream, Code) ->
    rtmp_session:stream_info(Session, Stream, Code).

stream_info(Session, Stream, Code, KeyVals) ->
    rtmp_session:stream_info(Session, Stream, Code, KeyVals).

%%====================================================================
%% Internal functions
%%====================================================================
