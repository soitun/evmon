-module(evmon_alarm).

-include_lib("elog/include/elog.hrl").

-export([start/0,
		stop/0]).

-behavior(gen_event).

-export([init/1, 
		handle_call/2,
		handle_event/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

start() ->
	ok = alarm_handler:add_alarm_handler(?MODULE, []).

stop() ->
	ok = alarm_handler:delete_alarm_handler(?MODULE).
	
init([]) ->
	?INFO("~p is startd.", [?MODULE]),
	{ok, state}.

handle_call(_Req, State) ->
	{ok, badreq, State}.

handle_event({set_alarm, Alarm}, State) ->
	?ERROR("alarm raise: ~p", [Alarm]),
	{ok, State};

handle_event({clear_alarm, AlarmId}, State) ->
	?INFO("alarm clear: ~p", [AlarmId]),
	{ok, State};

handle_event(Event, State) ->
	?INFO("unknow event: ~p", [Event]),
	{ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

