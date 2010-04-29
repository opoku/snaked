%%%-------------------------------------------------------------------
%%% File    : clock.erl
%%% Author  : ROAM
%%% Description : Just a clock ticker
%%%
%%% Created : 19 Mar 2010 by Osei Poku <opoku>
%%%-------------------------------------------------------------------
-module(clock).

-export([start/0,start/1,init/1,stop/0,set_tick/1]).
-include("game_state.hrl").

-define(FOOD_GENERATION_INTERVAL, 10).

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

get_new_foods(Tick) ->
    case Tick rem ?FOOD_GENERATION_INTERVAL of
	1 -> food:get_new_foods();
	_ -> []
    end.

get_new_player_positions() ->
    NewPos = game_logic:get_new_player_position(),
    %%io:format("DEBUG: in get_new_player_positions, before return NewPos -->~n ~p~n", [NewPos]),
    NewPos.

set_tick(NewTick) ->
    game_clock ! {set_tick, NewTick},
    ok.

loop(Time, Tick) ->
    receive
	{Pid, stop} ->
	    Pid ! ok,
	    io:format("Game Clock stopping~n");
	{forcetick} ->
	    game_manager:broadcast_tick(Tick),
	    loop(Time, Tick+1);
	{set_tick, T} ->
	    loop(Time, T)
    after
	Time ->
	    case game_manager:is_leader() of
		true ->
		    %% Generate food every FOOD_GENERATION_INTERVAL.
		    Options = case get_new_player_positions() of % each is {SnakeId, Position(a list of coords)}
				  [] -> [];
				  L -> [{newpos, L}]
			      end,
		    Options1 = case get_new_foods(Tick) of
				   [] -> Options;
				   NewFoods -> [{food, NewFoods}|Options]
			       end,
		    game_manager:broadcast_tick(Tick, Options1);
		_Else ->
		    %% TODO: maybe grab the current tick value from the gamelogic
		    nothing
	    end,
	    loop(Time, Tick+1)
    end.



