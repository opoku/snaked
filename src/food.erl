-module(food).
-compile([export_all]).
-include("common.hrl").
-include("game_state.hrl").
-undef(MODULE_DEBUG).
-define(MODULE_DEBUG,false).

%% This function generates a list of foods.
generate_foods(GameState) ->
    %% create a random number between 0 & no. of blocks on grid
    #game_state{size = {XSize, YSize}, clock = CurrentTick} = GameState,
    Offset = generate_random_number(XSize*YSize - 1),
    X = Offset rem XSize,
    Y = Offset div XSize,
    ?LOG_DEBUG("Attempting to generate food", #{position => {X, Y}, tick => CurrentTick}),
    case is_block_occupied(GameState, X, Y) of
        true ->
            ?LOG_DEBUG("Food position occupied, retrying", #{position => {X, Y}}),
            generate_foods(GameState);
        false ->
            AliveTill = CurrentTick + generate_random_interval(),
            NewFood = #food{position = [{X,Y}], value = 1, alive_till_tick = AliveTill},
            ?LOG_DEBUG("Food generated successfully", #{position => {X, Y}, value => 1, alive_till => AliveTill}),
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
    ?LOG("Yaaay New foods: ~p~n", [NewFoods]),
    case NewFoods of
        [] -> ok;
        _ -> ?LOG_DEBUG("New foods retrieved", #{food_count => length(NewFoods)})
    end,
    NewFoods.
