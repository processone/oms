%%%-------------------------------------------------------------------
%%% File    : oms_sm.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 12 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_sm).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 find_stream/2,
	 add_stream/3,
	 del_stream/2,
	 all_streams/1,
	 add_session/3,
	 del_session/2,
	 all_sessions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("p1_logger.hrl").

%%-define(USE_TRANSACTIONS, true).
-define(PROCNAME, ?MODULE).

-ifdef(USE_TRANSACTIONS).
-define(T(S),
 	case catch mnesia:transaction(fun() -> S end) of
 	    {atomic, Res} ->
 		Res;
 	    {_, Reason} ->
 		?ERROR_MSG("mnesia transaction failed: ~p", [Reason]),
 		{error, aborted}
 	end).
-else.
-define(T(S),
	case catch mnesia:sync_dirty(fun() -> S end) of
	    {'EXIT', Reason} ->
		?ERROR_MSG("mnesia sync_dirty failed: ~p", [Reason]),
		{error, aborted};
	    Res ->
		Res
	end).
-endif.

-record(oms_stream, {name, stream_ref, node}).
-record(oms_session, {name, addr}).
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?PROCNAME}, ?MODULE, [], []).

find_stream(AppName, Name) ->
    FullName = {AppName, iolist_to_binary(Name)},
    case mnesia:dirty_read({oms_stream, FullName}) of
	[#oms_stream{stream_ref = StreamRef}] ->
	    {ok, StreamRef};
	_ ->
	    {error, enoent}
    end.

add_stream(AppName, Name, StreamRef) ->
    FullName = {AppName, iolist_to_binary(Name)},
    ?T(case mnesia:read({oms_stream, FullName}) of
	   [] ->
	       mnesia:write(#oms_stream{name = FullName,
					node = node(),
					stream_ref = StreamRef});
	   _ ->
	       {error, duplicate}
       end).

del_stream(AppName, Name) ->
    FullName = {AppName, iolist_to_binary(Name)},
    ?T(mnesia:delete({oms_stream, FullName})).

all_streams(AppName) ->
    Streams = mnesia:dirty_select(
		oms_stream,
		[{#oms_stream{name = {AppName, '$1'}, _ = '_'},
		  [],
		  ['$_']}]),
    lists:map(
      fun(Stream) ->
	      {_, Name} = Stream#oms_stream.name,
	      StreamRef = Stream#oms_stream.stream_ref,
	      {binary_to_list(Name), StreamRef}
      end, Streams).

add_session(AppName, SID, Addr) ->
    FullName = {AppName, SID},
    ?T(case mnesia:read({oms_session, FullName}) of
	   [] ->
	       mnesia:write(#oms_session{name = FullName,
					 addr = Addr});
	   _ ->
	       {error, duplicate}
       end).

del_session(AppName, SID) ->
    FullName = {AppName, SID},
    ?T(mnesia:delete({oms_session, FullName})).

all_sessions(AppName) ->
    Sessions = mnesia:dirty_select(
		 oms_session,
		 [{#oms_session{name = {AppName, '$1'}, _ = '_'},
		   [],
		   ['$_']}]),
    lists:map(
      fun(Session) ->
	      {_, SID} = Session#oms_session.name,
	      Addr = Session#oms_session.addr,
	      {SID, Addr}
      end, Sessions).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    mnesia:create_table(oms_stream,
			[{ram_copies, [node()]},
			 {attributes,
			  record_info(fields, oms_stream)}]),
    mnesia:create_table(oms_session,
			[{ram_copies, [node()]},
			 {attributes,
			  record_info(fields, oms_session)}]),
    mnesia:add_table_copy(oms_stream, node(), ram_copies),
    mnesia:add_table_copy(oms_session, node(), ram_copies),
    mnesia:subscribe(system),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
clean_table_from_bad_node(Node) ->
    ?INFO_MSG("cleaning table from stopped node ~p", [Node]),
    ?T(begin
	   Es = mnesia:select(
		  oms_stream,
		  [{#oms_stream{node = Node, _ = '_'},
		    [],
		    ['$_']}]),
	   lists:foreach(fun(E) ->
				 mnesia:delete_object(E)
			 end, Es)
       end).
