%%%-------------------------------------------------------------------
%%% File    : clock.erl
%%% Author  : ROAM
%%% Description : Just a clock ticker
%%%
%%% Created : 19 Mar 2010 by Osei Poku <opoku>
%%%-------------------------------------------------------------------
-module(clock).

-export([start/0,start/1,init/1,stop/0,set_tick/1,pause/0,resume/0,resume/1]).
-include("common.hrl").
-include("game_state.hrl").

-define(CLOCK_TIME, 200).
-define(FOOD_GENERATION_INTERVAL, 10).

start() ->
    start(?CLOCK_TIME).

start(TimeOut) ->
    spawn(clock, init, [TimeOut]).

init(TimeOut) ->
    ?LOG_INFO("Game clock initialized", #{timeout => TimeOut}),
    register(game_clock, self()),
    loop(TimeOut).

stop() ->
    ?LOG_INFO("Stopping game clock"),
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

pause() ->
    ?LOG_DEBUG("Game clock paused"),
    game_clock ! {pause},
    ok.

resume() ->
    resume(?CLOCK_TIME).
resume(TimeOut) ->
    ?LOG_DEBUG("Game clock resumed", #{timeout => TimeOut}),
    game_clock ! {resume, TimeOut},
    ok.

get_new_player_positions() ->
    NewPos = game_logic:get_new_player_position(),
    %%?LOG("DEBUG: in get_new_player_positions, before return NewPos -->~n ~p~n", [NewPos]),
    NewPos.

set_tick(NewTick) ->
    ?LOG_DEBUG("Clock tick set", #{tick => NewTick}),
    game_clock ! {set_tick, NewTick},
    ok.

loop(Time, Tick) ->
    receive
	{Pid, stop} ->
	    Pid ! ok,
	    ?LOG("Game Clock stopping~n",[]);
	{forcetick} ->
	    game_manager:broadcast_tick(Tick),
	    loop(Time, Tick+1);
	{set_tick, T} ->
	    loop(Time, T);
	{pause} ->
	    loop(infinity, Tick);
	{resume, TimeOut} ->
	    loop(TimeOut, Tick)
    after
	Time ->
	    case game_manager:is_leader() of
		true ->
		    ?LOG_DEBUG("Clock tick (leader)", #{tick => Tick}),
		    %% Generate food every FOOD_GENERATION_INTERVAL.
		    Options = case get_new_player_positions() of % each is {SnakeId, Position(a list of coords)}
				  [] -> [];
				  L -> 
				      ?LOG_DEBUG("Broadcasting new player positions", #{player_count => length(L), tick => Tick}),
				      [{newpos, L}]
			      end,
		    Options1 = case get_new_foods(Tick) of
				   [] -> Options;
				   NewFoods -> 
				       ?LOG_DEBUG("Broadcasting new food", #{food_count => length(NewFoods), tick => Tick}),
				       [{food, NewFoods}|Options]
			       end,
		    game_manager:broadcast_tick(Tick, Options1);
		_Else ->
		    ?LOG_DEBUG("Clock tick (follower)", #{tick => Tick}),
		    %% TODO: maybe grab the current tick value from the gamelogic
		    nothing
	    end,
	    loop(Time, Tick+1)
    end.



