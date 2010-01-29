%%%-------------------------------------------------------------------
%%% File    : oms_lib.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 23 Jan 2010 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_lib).

%% API
-export([eprof_start/0, eprof_stop/0]).

%%====================================================================
%% API
%%====================================================================
eprof_start() ->
    eprof:start(),
    case lists:keyfind(running, 1, application:info()) of
	{_, Apps} ->
	    case lists:keyfind(oms, 1, Apps) of
		{_, Pid} when is_pid(Pid) ->
		    Procs = get_procs(Pid),
		    eprof:start_profiling(Procs);
		_ ->
		    {error, app_not_found}
	    end;
	_ ->
	    {error, no_app_info}
    end.

eprof_stop() ->
    eprof:stop_profiling(),
    eprof:analyse().

%%====================================================================
%% Internal functions
%%====================================================================
get_procs(Leader) ->
    lists:filter(
      fun(Pid) ->
	      case process_info(Pid, group_leader) of
		  {_, Leader} ->
		      true;
		  _ ->
		      false
	      end
      end, processes()).
