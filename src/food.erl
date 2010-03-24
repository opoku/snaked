-module(food).
-compile([export_all]).

-include("game_state.hrl").

%% This function generates a list of foods.
generate_foods(GameState) ->
    %% create a random number between 0 & no. of blocks on grid
    #game_state{size = {XSize, YSize}, clock = CurrentTick} = GameState,
    Offset = generate_random_number(XSize*YSize - 1),
    X = Offset rem XSize,
    Y = Offset div XSize,
    case is_block_occupied(GameState, X, Y) of
        true ->
            generate_foods(GameState);
        false ->
            NewFood = #food{position = [{X,Y}], value = 1, alive_till_tick = CurrentTick + generate_random_interval()},
            NewGameState = GameState#game_state{new_foods = [NewFood]},
            NewGameState
    end.

%% This function generates random interval (the interval for which the food should be alive).
generate_random_interval() ->
    50.
    
%% Generates a random integer from interval [0, N].
generate_random_number(N) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(N).

%% Find out if position {X,Y} on the canvas is occupied (by snake/food/obstacle).    
is_block_occupied(GameState, X, Y) ->
    #game_state{snakes=Snakes, foods = _Foods, obstacles = Obstacles} = GameState,
    ObstacleMap = game_logic:build_obstacle_map(Snakes ++ Obstacles),
    dict:is_key({X, Y}, ObstacleMap).

%% Returns the new food that was saved to GameState during last tick
get_new_foods() ->
    GameState = game_logic:get_game_state(),
    #game_state{new_foods = NewFoods} = GameState,
    io:format("Yaaay New foods: ~p~n", [NewFoods]),
    NewFoods.
