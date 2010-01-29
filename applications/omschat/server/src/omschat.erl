%%%-------------------------------------------------------------------
%%% File    : omschat.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 24 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(omschat).

%% API
-export([getmemberlist/1, publish/3, load/1, connect/2,
	 close/1, stream_started/3, stream_stopped/3]).

-include("oms.hrl").

%%====================================================================
%% API
%%====================================================================
load(_Opts) ->
    catch ets:new(omschat, [named_table, public]).

connect(#call_ctx{session = Session}, Nick) ->
    ets:insert(omschat, {Session, Nick}),
    ok.

close(#call_ctx{session = Session}) ->
    ets:delete(omschat, Session).

publish(_Ctx, "", _) ->
    {error, ?NS_PUBLISH_BAD_NAME,
     [{"description", "published stream should have a name"}]};
publish(#call_ctx{options = Opts}, Name, Type) ->
    case Type of
	"record" ->
	    MediaDir = proplists:get_value(media_dir, Opts, ""),
	    FileName = filename:join(MediaDir, Name ++ ".flv"),
	    {publish, Name, FileName};
	_ ->
	    {publish, Name}
    end.

stream_started(#call_ctx{app_name = AppName}, _StreamRef, Type) ->
    update_member_list(AppName, Type).

stream_stopped(#call_ctx{app_name = AppName}, _StreamRef, Type) ->
    update_member_list(AppName, Type).

getmemberlist(#call_ctx{app_name = AppName}) ->
    {result, {array, get_all_streams(AppName)}}.

%%====================================================================
%% Internal functions
%%====================================================================
get_all_streams(AppName) ->
    [StreamName || {StreamName, _} <- oms_api:all_streams(AppName)].

update_member_list(AppName, publish) ->
    Streams = {array, get_all_streams(AppName)},
    Sessions = ets:match(omschat, {'$1', '_'}),
    lists:foreach(
      fun([Session]) ->
	      oms_api:call(Session, 'Updatememberlist', [Streams])
      end, Sessions);
update_member_list(_AppName, _Type) ->
    ok.
