%% @author hgx <huogang@4399.com>
%% @since 2012-07-09
%% @copyright 2012 web.4399.com
%% @doc msg_count description

%% 作用：


-module(msg_count).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([test_main/0]).
-endif.

%% --------------------------------------------------------------------
%% Define
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Data description
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Exported Functions
%% --------------------------------------------------------------------
-export([
		 count/0
		]).

%% ====================================================================
%% API Functions
%% ====================================================================
count() ->
	put(msg13112, []),
	put(msg13210, []),
	List = get_msg(),
	Count = do_count(List, []),
	?WARNING_MSG("Msg Count:-----------------------------~n~p", [Count]),
	?WARNING_MSG("Msg Count 13112:-----------------------~n~p", [get(msg13112)]),
	?WARNING_MSG("Msg Count 13210:-----------------------~n~p", [get(msg13210)]).

%% ====================================================================
%% Local Functions
%% ====================================================================
do_count([{_, <<>>} | List], CountList) ->
	do_count(List, CountList);
do_count([{_, <<_:16, _:8, Cmd:16, Bin/binary>>} | List], CountList) ->
	do_count_one(Cmd, Bin),
	NewCountList =
	case lists:keyfind(Cmd, 1, CountList) of
		false ->
			[{Cmd, 1} | CountList];
		{_, Count} ->
			lists:keystore(Cmd, 1, CountList, {Cmd, Count+1})
	end,
	do_count(List, NewCountList);
do_count([], CountList) ->
	CountList.

get_msg() ->
	msg_data:get_data().

do_count_one(13112, <<PlayerId:64, _/binary>>) ->
	CountList = get(msg13112),
	NewCountList =
	case lists:keyfind(PlayerId, 1, CountList) of
		false ->
			[{PlayerId, 1} | CountList];
		{_, Count} ->
			lists:keystore(PlayerId, 1, CountList, {PlayerId, Count+1})
	end,
	put(msg13112, NewCountList);
do_count_one(13210, <<PlayerId:64, ID:32>>) ->
	CountList = get(msg13210),
	NewCountList =
	case lists:keyfind({PlayerId, ID}, 1, CountList) of
		false ->
			[{{PlayerId, ID}, 1} | CountList];
		{_, Count} ->
			lists:keystore({PlayerId, ID}, 1, CountList, {{PlayerId, ID}, Count+1})
	end,
	put(msg13210, NewCountList);
%% do_count_one(13210, <<PlayerId:64, _:32>>) ->
%% 	CountList = get(msg13210),
%% 	NewCountList =
%% 	case lists:keyfind(PlayerId, 1, CountList) of
%% 		false ->
%% 			[{PlayerId, 1} | CountList];
%% 		{_, Count} ->
%% 			lists:keystore(PlayerId, 1, CountList, {PlayerId, Count+1})
%% 	end,
%% 	put(msg13210, NewCountList);
do_count_one(_, _) -> ok.

%% ====================================================================
%% Test
%% ====================================================================
-ifdef(TEST).
-spec test_main() -> ok.
%% @doc Test main function.
test_main() ->
	ok.

-endif.


