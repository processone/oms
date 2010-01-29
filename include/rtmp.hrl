%%%-------------------------------------------------------------------
%%% File    : rtmp.hrl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 27 Nov 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-record(rtmp_msg, {type,
		   stream,
		   timestamp,
		   chunk_stream,
		   data}).

-record(amf_cmd, {name, trid, body}).

-record(amf_so, {name, persistent, events}).

-define(DEFAULT_WINDOW_SIZE, 128 * 1024).

%% User Control Events
-define(STREAM_BEGIN, 0).
-define(STREAM_EOF, 1).
-define(STREAM_DRY, 2).
-define(SET_BUFFER_LENGTH, 3).
-define(STREAM_IS_RECORDED, 4).
-define(PING_REQUEST, 6).
-define(PING_RESPONSE, 7).

%% Shared Object Events
-define(SO_USE, 1).
-define(SO_RELEASE, 2).
-define(SO_REQUEST_CHANGE, 3).
-define(SO_CHANGE, 4).
-define(SO_SUCCESS, 5).
-define(SO_SEND_MESSAGE, 6).
-define(SO_STATUS, 7).
-define(SO_CLEAR, 8).
-define(SO_REMOVE, 9).
-define(SO_REQUEST_REMOVE, 10).
-define(SO_USE_SUCCESS, 11).

-define(CHKSIZE, 1).
-define(ABORT, 2).
-define(ACKNO, 3).
-define(USER_CONTROL, 4).
-define(ACKSIZE, 5).
-define(PEERBW, 6).

-define(AUDIO, 8).
-define(VIDEO, 9).
-define(AMF3_DATA, 15).
-define(AMF3_SO, 16).
-define(AMF3_CMD, 17).
-define(AMF0_DATA, 18).
-define(AMF0_SO, 19).
-define(AMF0_CMD, 20).
