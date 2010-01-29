%%%-------------------------------------------------------------------
%%% File    : mynetconnection.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created :  8 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(firstapp).

%% Std API
-export([play/2, publish/3]).

%% Internal API
-export([getlistofavmedia/1]).

-include("oms.hrl").

%%====================================================================
%% API
%%====================================================================
%% Standard calls
play(#call_ctx{options = Opts}, Name) ->
    MediaDir = proplists:get_value(media_dir, Opts, ""),
    FileName = filename:join(MediaDir, Name),
    {play_file, FileName}.

publish(#call_ctx{options = Opts}, Name, "record") ->
    MediaDir = proplists:get_value(media_dir, Opts, ""),
    FileName = filename:join(MediaDir, Name ++ ".flv"),
    {publish, Name, FileName};
publish(_Ctx, _, _) ->
    error.

%% Internal API
getlistofavmedia(#call_ctx{options = Opts}) ->
    MediaDir = proplists:get_value(media_dir, Opts, ""),
    FLVFiles = filename:join(MediaDir, "*.flv"),
    Array = {array, filelib:wildcard(FLVFiles)},
    {result, Array}.

%%====================================================================
%% Internal functions
%%====================================================================
