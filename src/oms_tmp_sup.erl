%%%-------------------------------------------------------------------
%%% File    : oms_tmp_sup.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created :  4 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_tmp_sup).

%% API
-export([start_link/2, init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link(Name, Module) ->
    supervisor:start_link({local, Name}, ?MODULE, Module).

init(Module) ->
    {ok, {{simple_one_for_one, 10, 1},
	  [{undefined, {Module, start_link, []},
	    temporary, brutal_kill, worker, [Module]}]}}.
