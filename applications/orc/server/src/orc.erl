%%%-------------------------------------------------------------------
%%% File    : mynetconnection.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 18 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(orc).

%% API
-export([load/1, connect/2, post/3, close/1]).

-include("oms.hrl").

%%====================================================================
%% API
%%====================================================================
load(_Opts) ->
    catch ets:new(orc, [named_table, public]).

connect(#call_ctx{session = Session}, Nick) ->
    ets:insert(orc, {Session, Nick}),
    ok.

close(#call_ctx{session = Session}) ->
    ets:delete(orc, Session).

post(_Ctx, Nick, Msg) ->
    Sessions = ets:match(orc, {'$1', '_'}),
    NewMsg = iolist_to_binary([Nick, ": ", Msg, "\n"]),
    lists:foreach(
      fun([Session]) ->
	      oms_api:call(Session, newline, [NewMsg])
      end, Sessions),
    {result, {array, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
