%% @author hgx <huogang@4399.com>
%% @since 2012-07-09
%% @copyright 2012 web.4399.com
%% @doc mod_ description

%% ×÷ÓÃ£º


-module(mod_).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").

%% --------------------------------------------------------------------
%% Define
%% --------------------------------------------------------------------
-record(state, {}).

%% --------------------------------------------------------------------
%% Exported Functions
%% --------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3,	handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External	exports
-export([
		 start_link/0
		]).

%% API exports
-export([
		]).
%% ====================================================================
%% External	functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Internal functions
%% ====================================================================
do_init() ->
	process_flag(trap_exit, true),
	misc:write_monitor_pid(self(),?MODULE, {}),
	misc:register(global, ?MODULE, self()),
	
	{ok, #state{}}.

%% --------------------------------------------------------------------
do_call(Info, _, State)	->
	?WARNING_MSG("~p call is not match:~w",[?MODULE, Info]),
	{reply,	ok,	State}.

%% --------------------------------------------------------------------
do_cast(Info, State) ->
	?WARNING_MSG("~p cast is not match:~w",[?MODULE, Info]),
	{noreply, State}.

%% --------------------------------------------------------------------
do_info(Info, State) ->
	?WARNING_MSG("~p info is not match:~w",[?MODULE, Info]),
	{noreply, State}.

%% --------------------------------------------------------------------
do_terminate(Reason, _Status) ->
	?DEBUG("exit ~p process [Reason:~p]~n",[?MODULE, Reason]),
	misc:delete_monitor_pid(self()),
	ok.

%% ====================================================================
%% API functions
%% ====================================================================


%% ====================================================================
%% Local functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description:	Initiates the server
%% Returns:	{ok, State}			 |
%%			{ok, State,	Timeout} |
%%			ignore				 |
%%			{stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	try
		do_init()
	catch
		_:Reason ->
			?WARNING_MSG("~p do_init is exception:~w~n,Info:~w",[?MODULE, Reason, do_init]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			{ok, 0}
	end.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description:	Handling call messages
%% Returns:	{reply,	Reply, State}		   |
%%			{reply,	Reply, State, Timeout} |
%%			{noreply, State}			   |
%%			{noreply, State, Timeout}	   |
%%			{stop, Reason, Reply, State}   | (terminate/2 is called)
%%			{stop, Reason, State}			 (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Info,_From,	State) ->
	try
		do_call(Info,_From,	State)
	catch
		_:Reason ->
			?WARNING_MSG("~p handle_call is exception:~w~n,Info:~w",[?MODULE, Reason, Info]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			{reply,	ok,	State}
	end.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description:	Handling cast messages
%% Returns:	{noreply, State}		  |
%%			{noreply, State, Timeout} |
%%			{stop, Reason, State}			 (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Info, State) ->
	try
		do_cast(Info, State)
	catch
		_:Reason ->
			?WARNING_MSG("~p handle_cast is exception:~w~n,Info:~w",[?MODULE, Reason, Info]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			{noreply, State}
	end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description:	Handling all non call/cast messages
%% Returns:	{noreply, State}		  |
%%			{noreply, State, Timeout} |
%%			{stop, Reason, State}			 (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	try
		do_info(Info, State)
	catch
		_:Reason ->
			?WARNING_MSG("~p handle_info is exception:~w~n,Info:~w",[?MODULE, Reason, Info]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			{noreply, State}
	end.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description:	Shutdown the server
%% Returns:	any	(ignored by	gen_server)
%% --------------------------------------------------------------------
terminate(Reason, Status) ->
	try
		do_terminate(Reason, Status)
	catch
		_:Reason ->
			?WARNING_MSG("~p terminate is exception:~w~n",[?MODULE, Reason]),
			?WARNING_MSG("get_stacktrace:~p",[erlang:get_stacktrace()]),
			ok
	end.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose:	Convert	process	state when code	is changed
%% Returns:	{ok, NewState}
%% --------------------------------------------------------------------
code_change(_oldvsn, Status, _extra) ->
	{ok, Status}.

%% ====================================================================
%% Test
%% ====================================================================
-ifdef(TEST).
-spec test_main() -> ok.
%% @doc Test main function.
test_main() ->
	ok.

-endif.

