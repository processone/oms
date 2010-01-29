%%%-------------------------------------------------------------------
%%% File    : flowplayer.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created :  8 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(flowplayer).

%% Std API
-export([play/2]).

-include("oms.hrl").

%%====================================================================
%% API
%%====================================================================
%% Standard calls
play(#call_ctx{options = Opts}, Name) ->
    MediaDir = proplists:get_value(media_dir, Opts, ""),
    FileName = filename:join(MediaDir, Name),
    {play_file, FileName}.

%%====================================================================
%% Internal functions
%%====================================================================
