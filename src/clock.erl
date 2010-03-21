%%%-------------------------------------------------------------------
%%% File    : clock.erl
%%% Author  : ROAM
%%% Description : Just a clock ticker
%%%
%%% Created : 19 Mar 2010 by Osei Poku <opoku>
%%%-------------------------------------------------------------------
-module(clock).

-export([start/0,start/1,init/1,stop/0]).


start() ->
    start(200).

start(TimeOut) ->
    spawn(clock, init, [TimeOut]).

init(TimeOut) ->
    register(game_clock, self()),
    loop(TimeOut).

stop() ->
    game_clock ! {self(), stop},
    receive
	_ ->
	    ok
    end.

loop(Time) ->
	loop(Time, 1).

%% loop(_, 10) ->
%% 	done;

loop(Time, Tick) ->
    receive
	{Pid, stop} ->
	    Pid ! ok,
	    io:format("Game Clock stopping~n");
	{forcetick} ->
	    game_manager:broadcast_tick(Tick),
	    loop(Time, Tick+1)
    after Time ->
	    game_manager:broadcast_tick(Tick),
	    loop(Time, Tick+1)
    end.



