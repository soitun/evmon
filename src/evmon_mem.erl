%%%----------------------------------------------------------------------
%%% File    : evmon_mem.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : erlang vm memory monitor
%%% Created : 16 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(evmon_mem).

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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	?INFO("~p is started.", [?MODULE]),
    {ok, #state{}}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

