%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is ProcessOne SARL.
%% Portions created by ProcessOne are Copyright 2009, ProcessOne SARL.
%% All Rights Reserved.''
%%
%%     $Id: oms_xmlappconfig_parser.erl 150 2009-09-21 16:16:42Z mremond $
%%

%%%----------------------------------------------------------------------
%%% File    : p1_loglevel.erl
%%% Purpose : Loglevel switcher.
%%%           Be careful: you should not have any error_logger module
%%%           as p1_loglevel switcher is compiling and loading
%%%           dynamically a "virtual" error_logger module (Described
%%%           in a string at the end of this module).
%%%----------------------------------------------------------------------

-module(p1_loglevel).

-export([set/1]).

-include("p1_logger.hrl").

-define(LOGMODULE, "error_logger").

%% Error levels:
-define(LOG_LEVELS,
	[{0, no_log, "No log"}, {1, critical, "Critical"},
	 {2, error, "Error"}, {3, warning, "Warning"},
	 {4, info, "Info"}, {5, debug, "Debug"}]).

set(LogLevel) when is_atom(LogLevel) ->
    set(level_to_integer(LogLevel));
set(Loglevel) when is_integer(Loglevel) ->
    try {Mod, Code} =
	    dynamic_compile:from_string(p1_logger_src(Loglevel)),
	code:load_binary(Mod, (?LOGMODULE) ++ ".erl", Code)
    catch
      Type:Error ->
	  ?CRITICAL_MSG("Error compiling logger (~p): ~p~n",
			[Type, Error])
    end;
set(_) -> exit("Loglevel must be an integer").

level_to_integer(Level) ->
    case lists:keyfind(Level, 2, ?LOG_LEVELS) of
      {Int, Level, _Desc} -> Int;
      _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the p1 logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
p1_logger_src(Loglevel) ->
    L = integer_to_list(Loglevel),
    "-module(p1_logger).\n\n    -export([debug_msg"
    "/4,\n             info_msg/4,\n     "
    "        warning_msg/4,\n            "
    " error_msg/4,\n             critical_msg/4]).\n\n "
    "   %% Helper functions\n    debug_msg(Module, "
    "Line, Format, Args) when "
      ++
      L ++
	" >= 5 ->\n            notify(info_msg,\n "
	"                  \"D(~p:~p:~p) : \"++Format+"
	"+\"~n\",\n                   [self(), "
	"Module, Line]++Args);\n    debug_msg(_,_,_,_) "
	"-> ok.\n\n    info_msg(Module, Line, "
	"Format, Args) when "
	  ++
	  L ++
	    " >= 4 ->\n            notify(info_msg,\n "
	    "                  \"I(~p:~p:~p) : \"++Format+"
	    "+\"~n\",\n                   [self(), "
	    "Module, Line]++Args);\n    info_msg(_,_,_,_) "
	    "-> ok.\n\n    warning_msg(Module, Line, "
	    "Format, Args) when "
	      ++
	      L ++
		" >= 3 ->\n            notify(error,\n "
		"                  \"W(~p:~p:~p) : \"++Format+"
		"+\"~n\",\n                   [self(), "
		"Module, Line]++Args);\n    warning_msg(_,_,_,"
		"_) -> ok.\n\n    error_msg(Module, Line, "
		"Format, Args) when "
		  ++
		  L ++
		    " >= 2 ->\n            notify(error,\n "
		    "                  \"E(~p:~p:~p) : \"++Format+"
		    "+\"~n\",\n                   [self(), "
		    "Module, Line]++Args);\n    error_msg(_,_,_,_) "
		    "-> ok.\n\n    critical_msg(Module, Line, "
		    "Format, Args) when "
		      ++
		      L ++
			" >= 1 ->\n            notify(error,\n "
			"                  \"C(~p:~p:~p) : \"++Format+"
			"+\"~n\",\n                   [self(), "
			"Module, Line]++Args);\n    critical_msg(_,_,_"
			",_) -> ok.\n\n    %% Distribute the "
			"message to the Erlang error logger\n "
			"   notify(Type, Format, Args) ->\n  "
			"          LoggerMsg = {Type, group_leader(), "
			"{self(), Format, Args}},\n          "
			"  gen_event:notify(error_logger, LoggerMsg).\n "
			"   ".
