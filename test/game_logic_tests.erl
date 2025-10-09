-module(game_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("game_state.hrl").

advance_clock_increments_test() ->
    GS0 = #game_state{clock = 5},
    GS1 = game_logic:advance_clock(GS0),
    ?assertEqual(6, GS1#game_state.clock),
    GS2 = game_logic:advance_clock(GS0, 3),
    ?assertEqual(8, GS2#game_state.clock).

remove_stale_foods_test() ->
    F1 = #food{position = [{1,1}], value = 1, alive_till_tick = 3},
    F2 = #food{position = [{2,2}], value = 1, alive_till_tick = 4},
    Kept = game_logic:remove_stale_foods([F1, F2], 4),
    ?assertEqual([F2], Kept).

feed_snake_and_score_test() ->
    S0 = #snake{position = queue:from_list([{1,1},{1,2}]), length = 2, score = 0},
    F  = #food{position = [{1,1}], value = 2, alive_till_tick = 10},
    {Result, {S1, Foods1}} = game_logic:feed_snake(S0, [F]),
    ?assertEqual(fed, Result),
    ?assertEqual(4, S1#snake.length),
    ?assertEqual(100, S1#snake.score),
    ?assertEqual([], Foods1).

process_options_appends_food_test() ->
    F  = #food{position = [{1,1}], value = 1, alive_till_tick = 10},
    GS0 = #game_state{foods = []},
    GS1 = game_logic:process_options(GS0, [{food, [F]}]),
    ?assertEqual([F], GS1#game_state.foods).

detect_collision_on_obstacle_test() ->
    S   = #snake{id = foo, position = queue:from_list([{3,3},{3,4}])},
    Obs = [#object{type = obstacle, position = [{3,3}]}],
    Map = game_logic:build_obstacle_map([S] ++ Obs),
    ?assertEqual(true, game_logic:detect_collision(S, Map)).
