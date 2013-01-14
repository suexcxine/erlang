%% @author hgx <huogang@4399.com>
%% @since 2013-01-14
%% @copyright 2013 web.4399.com
%% @doc mod_war_peach description

%% 作用：新版蟠桃会 管理器


-module(mod_war_peach).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").

%% --------------------------------------------------------------------
%% Define
%% --------------------------------------------------------------------
-record(state, 
		{
		 status = over,
		 enter_time = 0,
		 over_time = 0,
		 war_scene = []
		}).

%% war_scene = [{pid, blueteam, redteam}...]
%% --------------------------------------------------------------------
%% Exported Functions
%% --------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3,	handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External	exports
-export([
		 start_link/0
		]).

%% 时间回调接口
-export([
		 tc_join/1,
		 tc_enter/2,
		 tc_over/0,
		 
		 on_show_log/0
		]).

%% 玩家调用接口
-export([
		 on_get_status/1,
		 on_join/1,
		 on_get_safe_scene/0,
		 on_enter/1,
		 on_get_war_scene/1
		]).

%% 战场消息控制
-export([call/2, cast/2]).
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
do_call({on_get_war_scene, PlayerId}, From, State) ->
	{reply,	0,	State};

do_call(Info, _, State)	->
	?WARNING_MSG("~p call is not match:~w",[?MODULE, Info]),
	{reply,	ok,	State}.

%% --------------------------------------------------------------------
%% 时间回调接口
do_cast({tc_join, Timer}, State) when State#state.status =:= over ->
	Status = join,
%% 	{ok, Bin} = ?PT_MAP:write(?SC_WAR_START_TIME, [?WAR_TENLIFE_KIND, Timer, 0]),
%% 	lib_send:send_to_all(Bin),
	RealTimer = Timer + misc_timer:now_seconds(),
	misc:set_kv(?KV_PEACH_STSTUS, Status),
	{noreply, State#state{status = Status, enter_time = RealTimer}};

do_cast({tc_enter, Timer}, State) when State#state.status =:= join ->
	Status = enter,
%% 	{ok, Bin} = ?PT_MAP:write(?SC_WAR_ENTERMSG, [?WAR_TENLIFE_KIND, 0]),
%% 	lib_send:send_to_all(Bin),
	Scene = do_start_war_scene(State),
	RealTimer = Timer + misc_timer:now_seconds(),
	{noreply, State#state{
						  status = Status,
						  over_time = RealTimer, 
						  war_scene = Scene
						 }};

do_cast({tc_over}, State) when State#state.status =:= enter ->
	Status = over,
	do_close_war_scene(State),
	%% 发送结束消息
%% 	{ok, Bin} = ?PT_MAP:write(?SC_WAR_CLOSE, [?WAR_TENLIFE_KIND]),
%% 	lib_send:send_to_all(Bin),
	misc:set_kv(?KV_PEACH_STSTUS, Status),
	{noreply, State#state{status = Status, 
						  war_scene = []}};

do_cast({on_show_log}, State) ->
	?WARNING_MSG("~n--status:[ ~p ]~n--war   :[~p]~n--------------------",
				 [State#state.status, State#state.war_scene]),
	{noreply, State};

do_cast({on_get_status, SendPid}, State) ->
	Timer = max(0, State#state.enter_time - misc_timer:now_seconds()),
%% 	{ok, Bin} = ?PT_MAP:write(?SC_WAR_START_TIME, [?WAR_TENLIFE_KIND, Timer, 0]),
%% 	lib_send:send_to_sid(SendPid, Bin),
	{noreply, State};

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
%% 时间回调接口
tc_join(Timer) ->
	gen_server:cast(?MODULE, {tc_join, Timer}).

tc_enter(Num, Timer) ->
	gen_server:cast(?MODULE, {tc_enter, Num, Timer}).

tc_over() ->
	gen_server:cast(?MODULE, {tc_over}).

on_show_log() ->
	gen_server:cast(?MODULE, {on_show_log}).

%% 玩家调用接口
%% --------------------------------------------------------------------
%% 获取战场状态
on_get_status(SendPid) ->
	case misc:get_kv(?KV_PEACH_STSTUS) of
		join ->
			gen_server:cast(?MODULE, {on_get_status, SendPid});
		enter ->
			{ok, Bin} = ?PT_MAP:write(?SC_WAR_ENTERMSG, [?WAR_TENLIFE_KIND, 0]),
			lib_send:send_to_sid(SendPid, Bin);
		_ ->
			ok
	end.

%% 进入安全场景
on_join(Player) ->
	Level = war_config:get_tenlife_level(),
	case {Level > Player#ets_players.level, misc:get_kv(?KV_PEACH_STSTUS)} of
		{false, enter} ->
			{X, Y} = war_config:get_tenlife_safe_pos(),
			{?WAR_TENLIFE_SAFE_MAPID, X, Y};
		{true, _} ->
			?WAR_MSG_TENLIFE_LEVEL;
		_ ->
			?WAR_MSG_TENLIFE_OVER
	end.

%% 获取安全场景信息
on_get_safe_scene() ->
	gen_server:call(?MODULE, {on_get_safe_scene}).

%% 进入战斗场景
on_enter(Player) ->
	case on_get_war_scene(Player#ets_players.player_id) of
		{_MapPid, _MapOnlyId, Group} ->
			{X, Y} = do_get_war_scene_pos(Group),
			{?WAR_TENLIFE_MAPID, X, Y};
		MsgId ->
			MsgId
	end.

%% 获取战斗场景信息
on_get_war_scene(PlayerId) ->
	gen_server:call(?MODULE, {on_get_war_scene, PlayerId}).

%% --------------------------------------------------------------------
%% 消息控制
call(Pid, Request) ->
	gen_server:call(Pid, {war_peach, Request}).
cast(Pid, Request) ->
	gen_server:cast(Pid, {war_peach, Request}).
%% ====================================================================
%% Local functions
%% ====================================================================
%% --------------------------------------------------------------------
%% 启动战斗场景
do_start_war_scene(State) ->
	[].

%% --------------------------------------------------------------------
%% 关闭战斗场景
do_close_war_scene(State) ->
	ok.

%% --------------------------------------------------------------------
%% 获取战斗场景坐标
do_get_war_scene_pos(Group) ->
	Rand = util:rand(1, 3) + ((Group - 1) * 3),
	war_config:get_tenlife_war_pos(Rand).

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

