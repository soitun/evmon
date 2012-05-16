-module(evmon_app).

-export([start/0]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
	[application:start(App) || App <- [sasl, elog, evmon]].

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
	evmon_alarm:start(),
    evmon_sup:start_link().

stop(_State) ->
	evmon_alarm:stop(),
    ok.

