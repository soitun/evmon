%%%----------------------------------------------------------------------
%%% File    : evmon_mem.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : erlang vm memory monitor
%%% Created : 16 May. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------

%%LICENSE NOTICE: some code copied from rabbitmq vm_memory_monitor.erl.

-module(evmon_mem).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-export([start_link/0]).

-export([get_total_memory/0,
		 get_vm_limit/0,
         get_check_interval/0,
		 set_check_interval/1,
         get_vm_memory_high_watermark/0,
		 set_vm_memory_high_watermark/1,
         get_memory_limit/0]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-define(DEFAULT_MEMORY_CHECK_INTERVAL, 1000).
-define(ONE_MB, 1048576).

%% For an unknown OS, we assume that we have 1GB of memory. It'll be
%% wrong. Scale by vm_memory_high_watermark in configuration to get a
%% sensible value.
-define(MEMORY_SIZE_FOR_UNKNOWN_OS, 2147483648).

-record(state, {total_memory,
                memory_limit,
                timeout,
                timer,
                alarmed
               }).

get_total_memory() ->
    get_total_memory(os:type()).

get_vm_limit() ->
    get_vm_limit(os:type()).

get_check_interval() ->
    gen_server:call(?MODULE, get_check_interval, infinity).

set_check_interval(Fraction) ->
    gen_server:call(?MODULE, {set_check_interval, Fraction}, infinity).

get_vm_memory_high_watermark() ->
    gen_server:call(?MODULE, get_vm_memory_high_watermark, infinity).

set_vm_memory_high_watermark(Fraction) ->
    gen_server:call(?MODULE, {set_vm_memory_high_watermark, Fraction},
                    infinity).

get_memory_limit() ->
    gen_server:call(?MODULE, get_memory_limit, infinity).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	MemFraction = 
	case application:get_env(vm_memory_high_watermark) of
	{ok, Val} -> Val;
	undefined -> 0.1
	end,
    TRef = start_timer(?DEFAULT_MEMORY_CHECK_INTERVAL),
    State = #state { timeout = ?DEFAULT_MEMORY_CHECK_INTERVAL,
                     timer = TRef,
                     alarmed = false},
	?INFO("~p is started.", [?MODULE]),
    {ok, set_mem_limits(State, MemFraction)}.

handle_call(get_vm_memory_high_watermark, _From, State) ->
    {reply, State#state.memory_limit / State#state.total_memory, State};

handle_call({set_vm_memory_high_watermark, MemFraction}, _From, State) ->
    State1 = set_mem_limits(State, MemFraction),
    ?INFO("Memory alarm changed to ~p, ~p bytes.~n",
                          [MemFraction, State1#state.memory_limit]),
    {reply, ok, State1};

handle_call(get_check_interval, _From, State) ->
    {reply, State#state.timeout, State};

handle_call({set_check_interval, Timeout}, _From, State) ->
    {ok, cancel} = timer:cancel(State#state.timer),
    {reply, ok, State#state{timeout = Timeout, timer = start_timer(Timeout)}};

handle_call(get_memory_limit, _From, State) ->
    {reply, State#state.memory_limit, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(update, State) ->
    {noreply, internal_update(State)};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------

set_mem_limits(State, MemFraction) ->
    TotalMemory =
        case get_total_memory() of
            unknown ->
                case State of
                    #state { total_memory = undefined,
                             memory_limit = undefined } ->
                        ?WARNING(
                          "Unknown total memory size for your OS ~p. "
                          "Assuming memory size is ~pMB.~n",
                          [os:type(),
                           trunc(?MEMORY_SIZE_FOR_UNKNOWN_OS/?ONE_MB)]);
                    _ ->
                        ok
                end,
                ?MEMORY_SIZE_FOR_UNKNOWN_OS;
            M -> M
        end,
    MemLim = get_mem_limit(MemFraction, TotalMemory),
    ?INFO("Memory limit set to ~pMB of ~pMB total.~n",
                          [trunc(MemLim/?ONE_MB), trunc(TotalMemory/?ONE_MB)]),
    internal_update(State #state { total_memory = TotalMemory,
                                   memory_limit = MemLim }).

internal_update(State = #state { memory_limit = MemLimit,
                                 alarmed = Alarmed}) ->
    MemUsed = erlang:memory(total),
    NewAlarmed = MemUsed > MemLimit,
    case {Alarmed, NewAlarmed} of
        {false, true} ->
            emit_update_info(set, MemUsed, MemLimit),
            alarm_handler:set_alarm({{vm_memory_high_watermark, node()}, []});
        {true, false} ->
            emit_update_info(clear, MemUsed, MemLimit),
            alarm_handler:clear_alarm({vm_memory_high_watermark, node()});
        _ ->
            ok
    end,
    State #state {alarmed = NewAlarmed}.

emit_update_info(State, MemUsed, MemLimit) ->
    ?INFO("vm_memory_high_watermark ~p. Memory used:~p allowed:~p~n",
		[State, MemUsed, MemLimit]).

start_timer(Timeout) ->
    {ok, TRef} = timer:send_interval(Timeout, update),
    TRef.

%% According to http://msdn.microsoft.com/en-us/library/aa366778(VS.85).aspx
%% Windows has 2GB and 8TB of address space for 32 and 64 bit accordingly.
get_vm_limit({win32,_OSname}) ->
    case erlang:system_info(wordsize) of
        4 -> 2*1024*1024*1024;          %% 2 GB for 32 bits  2^31
        8 -> 8*1024*1024*1024*1024      %% 8 TB for 64 bits  2^42
    end;

%% On a 32-bit machine, if you're using more than 2 gigs of RAM you're
%% in big trouble anyway.
get_vm_limit(_OsType) ->
    case erlang:system_info(wordsize) of
        4 -> 4*1024*1024*1024;          %% 4 GB for 32 bits  2^32
        8 -> 256*1024*1024*1024*1024    %% 256 TB for 64 bits 2^48
             %%http://en.wikipedia.org/wiki/X86-64#Virtual_address_space_details
    end.

get_mem_limit(MemFraction, TotalMemory) ->
    AvMem = lists:min([TotalMemory, get_vm_limit()]),
    trunc(AvMem * MemFraction).

%%----------------------------------------------------------------------------
%% Internal Helpers
%%----------------------------------------------------------------------------
cmd(Command) ->
    Exec = hd(string:tokens(Command, " ")),
    case os:find_executable(Exec) of
        false -> throw({command_not_found, Exec});
        _     -> os:cmd(Command)
    end.

%% get_total_memory(OS) -> Total
%% Windows and Freebsd code based on: memsup:get_memory_usage/1
%% Original code was part of OTP and released under "Erlang Public License".

get_total_memory({unix,darwin}) ->
    File = cmd("/usr/bin/vm_stat"),
    Lines = string:tokens(File, "\n"),
    Dict = dict:from_list(lists:map(fun parse_line_mach/1, Lines)),
    [PageSize, Inactive, Active, Free, Wired] =
        [dict:fetch(Key, Dict) ||
            Key <- [page_size, 'Pages inactive', 'Pages active', 'Pages free',
                    'Pages wired down']],
    PageSize * (Inactive + Active + Free + Wired);

get_total_memory({unix,freebsd}) ->
    PageSize  = sysctl("vm.stats.vm.v_page_size"),
    PageCount = sysctl("vm.stats.vm.v_page_count"),
    PageCount * PageSize;

get_total_memory({unix,openbsd}) ->
    sysctl("hw.usermem");

get_total_memory({win32,_OSname}) ->
    %% Due to the Erlang print format bug, on Windows boxes the memory
    %% size is broken. For example Windows 7 64 bit with 4Gigs of RAM
    %% we get negative memory size:
    %% > os_mon_sysinfo:get_mem_info().
    %% ["76 -1658880 1016913920 -1 -1021628416 2147352576 2134794240\n"]
    %% Due to this bug, we don't actually know anything. Even if the
    %% number is postive we can't be sure if it's correct. This only
    %% affects us on os_mon versions prior to 2.2.1.
    case application:get_key(os_mon, vsn) of
        undefined ->
            unknown;
        {ok, Version} ->
            case rabbit_misc:version_compare(Version, "2.2.1", lt) of
                true -> %% os_mon is < 2.2.1, so we know nothing
                    unknown;
                false ->
                    [Result|_] = os_mon_sysinfo:get_mem_info(),
                    {ok, [_MemLoad, TotPhys, _AvailPhys,
                          _TotPage, _AvailPage, _TotV, _AvailV], _RestStr} =
                        io_lib:fread("~d~d~d~d~d~d~d", Result),
                    TotPhys
            end
    end;

get_total_memory({unix, linux}) ->
    File = read_proc_file("/proc/meminfo"),
    Lines = string:tokens(File, "\n"),
    Dict = dict:from_list(lists:map(fun parse_line_linux/1, Lines)),
    dict:fetch('MemTotal', Dict);

get_total_memory({unix, sunos}) ->
    File = cmd("/usr/sbin/prtconf"),
    Lines = string:tokens(File, "\n"),
    Dict = dict:from_list(lists:map(fun parse_line_sunos/1, Lines)),
    dict:fetch('Memory size', Dict);

get_total_memory({unix, aix}) ->
    File = cmd("/usr/bin/vmstat -v"),
    Lines = string:tokens(File, "\n"),
    Dict = dict:from_list(lists:map(fun parse_line_aix/1, Lines)),
    dict:fetch('memory pages', Dict) * 4096;

get_total_memory(_OsType) ->
    unknown.

%% A line looks like "Foo bar: 123456."
parse_line_mach(Line) ->
    [Name, RHS | _Rest] = string:tokens(Line, ":"),
    case Name of
        "Mach Virtual Memory Statistics" ->
            ["(page", "size", "of", PageSize, "bytes)"] =
                string:tokens(RHS, " "),
            {page_size, list_to_integer(PageSize)};
        _ ->
            [Value | _Rest1] = string:tokens(RHS, " ."),
            {list_to_atom(Name), list_to_integer(Value)}
    end.

%% A line looks like "FooBar: 123456 kB"
parse_line_linux(Line) ->
    [Name, RHS | _Rest] = string:tokens(Line, ":"),
    [Value | UnitsRest] = string:tokens(RHS, " "),
    Value1 = case UnitsRest of
                 [] -> list_to_integer(Value); %% no units
                 ["kB"] -> list_to_integer(Value) * 1024
             end,
    {list_to_atom(Name), Value1}.

%% A line looks like "Memory size: 1024 Megabytes"
parse_line_sunos(Line) ->
    case string:tokens(Line, ":") of
        [Name, RHS | _Rest] ->
            [Value1 | UnitsRest] = string:tokens(RHS, " "),
            Value2 = case UnitsRest of
                         ["Gigabytes"] ->
                             list_to_integer(Value1) * ?ONE_MB * 1024;
                         ["Megabytes"] ->
                             list_to_integer(Value1) * ?ONE_MB;
                         ["Kilobytes"] ->
                             list_to_integer(Value1) * 1024;
                         _ ->
                             Value1 ++ UnitsRest %% no known units
                     end,
            {list_to_atom(Name), Value2};
        [Name] -> {list_to_atom(Name), none}
    end.

%% Lines look like " 12345 memory pages"
%% or              "  80.1 maxpin percentage"
parse_line_aix(Line) ->
    [Value | NameWords] = string:tokens(Line, " "),
    Name = string:join(NameWords, " "),
    {list_to_atom(Name),
     case lists:member($., Value) of
         true  -> trunc(list_to_float(Value));
         false -> list_to_integer(Value)
     end}.

sysctl(Def) ->
    list_to_integer(cmd("/sbin/sysctl -n " ++ Def) -- "\n").

%% file:read_file does not work on files in /proc as it seems to get
%% the size of the file first and then read that many bytes. But files
%% in /proc always have length 0, we just have to read until we get
%% eof.
read_proc_file(File) ->
    {ok, IoDevice} = file:open(File, [read, raw]),
    Res = read_proc_file(IoDevice, []),
    file:close(IoDevice),
    lists:flatten(lists:reverse(Res)).

-define(BUFFER_SIZE, 1024).
read_proc_file(IoDevice, Acc) ->
    case file:read(IoDevice, ?BUFFER_SIZE) of
        {ok, Res} -> read_proc_file(IoDevice, [Res | Acc]);
        eof       -> Acc
    end.

