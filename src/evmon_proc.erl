%%%----------------------------------------------------------------------
%%% File    : evmon_proc.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : erlang vm memory monitor
%%% Created : 16 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evmon_proc).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-export([start_link/0]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {}).

-define(OPTIONS, [{long_gc, 500}, 
				  {large_heap, 5000000},
				  busy_port]).

-define(PROC_INFO, [status,
					registered_name,
				    message_queue_len, 
					memory, 
					min_heap_size,
					min_bin_vheap_size,
					total_heap_size, 
					heap_size, 
					reductions,
					stack_size,
					dictionary]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	Options =
	case application:get_env(proc) of
	{ok, Val} -> Val;
	undefined -> ?OPTIONS
	end,
    erlang:system_monitor(self(), Options),
	?INFO("~p is started.", [?MODULE]),
    {ok, #state{}}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({monitor, Pid, long_gc, Info}, State) ->
    ?ERROR("long_gc error: pid=~p, info=~p", [Pid, Info]),
	case erlang:is_proess_alive(Pid) of
	true ->
		Info = erlang:process_info(Pid, ?PROC_INFO),
		?ERROR("long_gc process info: ~n~p", [Info]);
	false ->
		?ERROR_MSG("long_gc pid '~p' is dead")
	end,
    {noreply, State};

handle_info({monitor, Pid, large_heap, Info}, State) ->
    ?ERROR("large_heap error: pid=~p, info=~p", [Pid, Info]),
	case erlang:is_proess_alive(Pid) of
	true ->
		Info = erlang:process_info(Pid, ?PROC_INFO),
		?ERROR("large_heap process info: ~n~p", [Info]);
	false ->
		?ERROR_MSG("large_heap pid '~p' is dead")
	end,
    {noreply, State};

handle_info({monitor, SusPid, busy_port, Port}, State) ->
    ?ERROR("busy_port error: pid=~p, port=~p", [SusPid, Port]),
    ?ERROR("busy port info: ~p", [erlang:port_info(Port)]), 
    ?ERROR("busy process: ~p", [erlang:process_display(SusPid, backtrace)]),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


