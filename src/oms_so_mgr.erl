%%%-------------------------------------------------------------------
%%% File    : oms_so_mgr.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 18 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_so_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0, process/3, close/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("p1_logger.hrl").
-include("rtmp.hrl").

-define(T(S),
	case catch mnesia:transaction(fun() -> S end) of
	    {atomic, Res} ->
		Res;
	    {_, Reason} ->
		?ERROR_MSG("mnesia transaction failed: ~p", [Reason]),
		{error, aborted}
	end).

-record(oms_so, {name, object, sessions, persistent}).
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process(AppName, Session,
	#amf_so{name = Name,
		persistent = IsPersistent,
		events = [{?SO_USE, _}]}) ->
    FullName = fullname(AppName, Name),
    ?T(begin
	   {NewSO, Reply} =
	       case mnesia:read({oms_so, FullName}) of
		   [#oms_so{object = Obj, sessions = Sessions} = SO] ->
		       Events = [{?SO_USE_SUCCESS, []}] ++
			   [{?SO_CHANGE, [KeyVal]} || KeyVal <- Obj],
		       {SO#oms_so{sessions = [Session | Sessions]},
			#amf_so{name = Name,
				persistent = IsPersistent,
				events = Events}};
		   [] ->
		       {#oms_so{name = FullName,
				sessions = [Session],
				object = [],
				persistent = IsPersistent},
			#amf_so{name = Name,
				persistent = IsPersistent,
				events = [{?SO_USE_SUCCESS, []}]}}
	       end,
	   rtmp_session:route(Session, Reply),
	   mnesia:write(NewSO),
	   {add, Name}
       end);
process(AppName, _Session,
	#amf_so{name = Name,
		events = [{?SO_REQUEST_CHANGE, KeyVals}]}) ->
    FullName = fullname(AppName, Name),
    ?T(case mnesia:read({oms_so, FullName}) of
	   [#oms_so{object = Obj, persistent = IsPersistent,
		    sessions = Sessions} = SO] ->
	       NewObj = lists:ukeymerge(1, KeyVals, Obj),
	       Events = [{?SO_CHANGE, [KeyVal]} || KeyVal <- NewObj],
	       AMFSO = #amf_so{name = Name,
			       persistent = IsPersistent,
			       events = Events},
	       lists:foreach(
		 fun(Session) ->
			 rtmp_session:route(Session, AMFSO)
		 end, Sessions),
	       mnesia:write(SO#oms_so{object = NewObj}),
	       ok;
	   [] ->
	       {error, enoent}
       end);
process(AppName, Session,
	#amf_so{name = Name,
		events = [{?SO_RELEASE, []}]} = SO) ->
    rtmp_session:route(Session, SO),
    close(AppName, Session, Name).

close(AppName, Session, Name) ->
    FullName = fullname(AppName, Name),
    ?T(begin
	   case mnesia:read({oms_so, FullName}) of
	       [#oms_so{sessions = Sessions,
			persistent = IsPersistent} = SO] ->
		   case lists:delete(Session, Sessions) of
		       [] when not IsPersistent ->
			   mnesia:delete_object(SO);
		       NewSessions ->
			   mnesia:write(SO#oms_so{sessions = NewSessions})
		   end;
	       [] ->
		   ok
	   end,
	   {del, Name}
       end).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    mnesia:create_table(
      oms_so,
      [{disc_copies, [node()]},
       {attributes, record_info(fields, oms_so)}]),
    mnesia:add_table_copy(oms_so, node(), disc_copies),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
fullname(AppName, ObjName) ->
    {AppName, ObjName}.
