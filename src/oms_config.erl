%%%-------------------------------------------------------------------
%%% File    : oms_config.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : 
%%%
%%% Created : 19 Dec 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(oms_config).

-behaviour(gen_server).

%% API
%%-compile(export_all).
-export([start_link/0, get_opts/1, get_opts/2,
	 get_opt/2, get_opt/3, parse/1, dump/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DEFAULT_CONFIG_PATH, "oms.cfg").

-include("p1_logger.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Path = get_config_path(),
    case parse(Path) of
	{ok, Config} ->
	    gen_server:start_link(
	      {local, ?MODULE}, ?MODULE, [Config], []);
	_Err ->
	    {stop, bad_config}
    end.

get_opts(Section) when is_atom(Section) ->
    case ets:lookup(oms_config, Section) of
	[] ->
	    [[]];
	Res ->
	    [Opts || {_, Opts} <- Res]
    end.

get_opts(Section, Config) when is_atom(Section) ->
    case proplists:get_all_values(Section, Config) of
	[] ->
	    [[]];
	Res ->
	    Res
    end.

get_opt(Name, Opts) ->
    get_opt(Name, Opts, undefined).

get_opt(Name, Opts, Default) ->
    case lists:keysearch(Name, 1, Opts) of
	{value, {_, Val}} ->
	    Val;
	_ ->
	    Default
    end.

parse(Path) ->
    case file:read_file(Path) of
	{ok, Data} ->
	    case do_parse(Data) of
		{ok, Config} ->
		    {ok, Config};
		Err ->
		    ?CRITICAL_MSG("~s", [format_error(Path, Err)]),
		    {error, bad_config}
	    end;
	Err ->
	    ?CRITICAL_MSG("~s", [format_error(Path, Err)]),
	    {error, bad_config}
    end.

dump(Config) ->
    lists:map(
      fun({Section, Opts}) ->
	      [$[, atom_to_list(Section), $], $\n,
	       [[atom_to_list(Key), " = ", Val, $\n] ||
		   {Key, Val} <- Opts], $\n]
      end, Config).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Config]) ->
    ets:new(oms_config, [named_table, public, bag]),
    lists:foreach(
      fun({Section, Opts}) ->
	      ets:insert(oms_config, {Section, Opts})
      end, Config),
    override_loglevel(),
    ?DEBUG("using configuration:~n~s", [dump(Config)]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_config_path() ->
    case application:get_env(oms, config) of
	{ok, Path} ->
	    Path;
	undefined ->
	    case os:getenv("OMS_CONFIG_PATH") of
		false ->
		    ?DEFAULT_CONFIG_PATH;
		Path ->
		    Path
	    end
    end.

override_loglevel() ->
    [Opts|_] = get_opts(main),
    case lists:keysearch(loglevel, 1, Opts) of
	{value, {_, Level}} ->
	    case catch list_to_integer(Level) of
		N when is_integer(N), N >= 0, N =< 5 ->
		    p1_loglevel:set(N);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Config parser. TODO: rewrite with leex and yeec
%%--------------------------------------------------------------------
do_parse(Data) ->
    Lines = read_lines(binary_to_list(Data)),
    NewLines = lists:flatmap(
		 fun({L, N}) ->
			 case remove_comments(strip(L)) of
			     "" ->
				 [];
			     L1 ->
				 [{L1, N, L}]
			 end
		 end, Lines),
    parse(NewLines, []).

read_lines(Data) ->
    {_, Lines} =
	lists:foldl(
	  fun(Line, {Num, Acc}) ->
		  {Num+1, [{Line, Num}|Acc]}
	  end, {1, []}, string:tokens(Data, "\n")),
    lists:reverse(Lines).

parse([{"[" ++ Rest, N, Orig}| T], Acc) ->
    case lists:last(Rest) of
	$] ->
	    Length = length(Rest),
	    case strip(string:substr(Rest, 1, Length-1)) of
		"" ->
		    {error, N, Orig};
		Section ->
		    parse_section(T, list_to_atom(Section), [], Acc)
	    end;
	_ ->
	    {error, N, Orig}
    end;
parse([], Acc) ->
    {ok, lists:reverse(Acc)};
parse([{_, N, Orig}|_], _Acc) ->
    {error, N, Orig}.

parse_section([{"[" ++ _, _, _}| _] = Lines, Section, Opts, Acc) ->
    parse(Lines, [{Section, lists:ukeysort(1, Opts)} | Acc]);
parse_section([{Line, N, Orig} | T], Section, Opts, Acc) ->
    case string:tokens(Line, "=") of
	[Key, Val] ->
	    case {strip(Key), strip(Val)} of
		{NewKey, NewVal} when NewKey /= "", NewVal /= "" ->
		    Key1 = list_to_atom(NewKey),
		    parse_section(
		      T, Section, [{Key1, NewVal}|Opts], Acc);
		_ ->
		    {error, N, Orig}
	    end;
	_ ->
	    {error, N, Orig}
    end;
parse_section([], Section, Opts, Acc) ->
    parse([], [{Section, lists:ukeysort(1, Opts)} | Acc]).

format_error(Path, {error, Reason}) ->
    io_lib:format(
      "unable to read file ~p: ~s",
      [Path, file:format_error(Reason)]);
format_error(Path, {error, N, Reason}) ->
    io_lib:format(
      "syntax error in ~p at line ~p: ~p",
      [Path, N, Reason]).

strip(S) ->
    strip1(strip1(S)).

strip1([$\n|T]) ->
    strip1(T);
strip1([$\r|T]) ->
    strip1(T);
strip1([$ |T]) ->
    strip1(T);
strip1([$\t|T]) ->
    strip1(T);
strip1(S) ->
    lists:reverse(S).

remove_comments(S) ->
    remove_comments(S, []).

remove_comments([$#|_], Acc) ->
    lists:reverse(Acc);
remove_comments([C|T], Acc) ->
    remove_comments(T, [C|Acc]);
remove_comments([], Acc) ->
    lists:reverse(Acc).
