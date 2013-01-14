

player_responser.erl

%% --------------------------------------------------------------------
%% 新版蟠桃会相关
do_call({war_peach, Info}, From, State) ->
	lib_war_peach_player:do_call(Info, From, State);

%% 新版蟠桃会相关
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 新版蟠桃会相关
do_cast({war_peach, Info}, State) ->
	lib_war_peach_player:do_cast(Info, State);

%% 新版蟠桃会相关
%% --------------------------------------------------------------------


mod_map.erl

%% --------------------------------------------------------------------
%% 新版蟠桃会相关			尝试使用新方法
do_call({war_peach, Info}, From, State) ->
	lib_war_peach_scene:do_call(Info, From, State);

%% 新版蟠桃会相关
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 新版蟠桃会相关			尝试使用新方法
do_cast({war_peach, Info}, State) ->
	lib_war_peach_scene:do_cast(Info, State);

%% 新版蟠桃会相关
%% --------------------------------------------------------------------



原理：包装 cast 和 call
mod_war_peach.erl

%% --------------------------------------------------------------------
%% 消息控制
call(Pid, Request) ->
	gen_server:call(Pid, {war_peach, Request}).
cast(Pid, Request) ->
	gen_server:cast(Pid, {war_peach, Request}).
