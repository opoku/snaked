-module(game_logic).
-compile([export_all]).

-import(queue, [out/1, out_r/1, in/2, in_r/2]).
-import(lists, [map/2]).

-include("game_state.hrl").

%% this module defines the game logic.  it basically evaluates game events and repositions
%% the snakes on screen.  

%% it receives two messages, the timer tick and the event.
%% it maintains a snake state which is a list of coordinates

%% snakestate is a list of snakes

%% size of the game_state defines how big the grid is

%% foods and obstacles are objects.  
%% the type of an obstacle is the atom obstacle

%% the type of a food is {food, food_type}, where food_type is one of [regular, magic].
%% Magic food makes you invincible for 5 ticks or something.  Will probably only do
%% regular food for now.

%% position is best described in the following way:
%%% position of head (x,y), length

start() ->
    Snake1 = #snake{id=one, direction=up},
    game_loop(#game_state{snakes=[Snake1]}).

game_loop(GameState) ->
    game_loop(GameState, queue:new()).

game_loop (GameState, QueuedMoves) ->
    #game_state{clock=Clock} = GameState,
    receive
	{print_state} ->
	    io:format("Clock: ~p~nGameState: ~p~nQueuedMoves: ~p~n", [Clock, GameState, QueuedMoves]),
	    game_loop(GameState, QueuedMoves);
	{tick, NewClock} ->
	    case Clock + 1 =:= NewClock of
		true ->
		    %% update the gui
		    GameState1 = move_snakes(GameState),
		    {GameState2, Results} = evaluate_snakes(GameState1), %% results is basically some messages saying what happened as a result of evaluation
		    GameState3 = advance_clock(GameState2),
		    display_board(GameState3, Results),
		    {GameState4, NewQueue} = process_queued_moves(GameState3, QueuedMoves),
		    game_loop(GameState4, NewQueue);
		false -> % ignore other 
		    nothing
	    end;
	{move, _SnakeId, _Direction, Clock} = Move ->
	    {NewGameState, NewQueuedMoves} = process_move(Move, GameState, QueuedMoves),
	    game_loop(NewGameState, NewQueuedMoves)
    end.

advance_clock(GS) ->
    advance_clock(GS, 1).

advance_clock(#game_state{clock=Clock} = GS, Steps) ->
    GS#game_state{clock=Clock+Steps}.


evaluate_snakes(GS) ->
    %% returns {NewGameState, Results}  where results is the deaths or foods obtained
    {GS1, Results1} = evaluate_obstacles(GS),
    {GS2, Results2} = evaluate_food(GS1),
    {GS2, Results1 ++ Results2}.


find_point_in_point_list(Point, PointList) ->
    lists:member(Point, PointList).

detect_collision(Snake, ObstacleMap) ->
    %% true or false
    #snake{position=SnakePos} = Snake,
    dict:is_key(front(SnakePos), ObstacleMap).

evaluate_obstacles(GS) ->
    %% returns {GS1, Results}
    #game_state{snakes=Snakes, obstacles=Obs} = GS,
    ObstacleMap = build_obstacle_map(Snakes ++ Obs),
    {DeadSnakes, AliveSnakes} = lists:partition(fun(Snake) -> detect_collision(Snake, ObstacleMap) end, Snakes),
    Results = map(fun(S) -> {killed, S#snake.id} end, DeadSnakes),
    {GS#game_state{snakes=AliveSnakes}, Results}.

evaluate_food(GS) ->
    %% returns {GS1, Results}
    #game_state{snakes=Snakes, foods=Foods} = GS,
    {Snakes1, Foods1, Results} = evaluate_food(Snakes, Foods),
    {GS#game_state{snakes=Snakes1, foods=Foods1}, Results}.

build_obstacle_map(Objects) ->
    build_obstacle_map(Objects, dict:new()).

build_obstacle_map([#snake{} = Snake | Others], D) ->
    build_obstacle_map(Others, add_snake_to_map(Snake, D));
build_obstacle_map([#object{position=ObjPos}| Others], D) ->
    build_obstacle_map(Others, add_obstacle_point_list_to_map(ObjPos,D));
build_obstacle_map([], D) ->
    D.

add_obstacle_point_list_to_map(PointList, D) ->
    lists:foldl(fun({Key, Val}, Dict) -> dict:append(Key, Val, Dict) end,
		D,
		[{Coord, obstacle} || Coord <- PointList]).

add_snake_to_map(#snake{position=SnakePos, id = Id}, D) ->
    [SnakeHead | SnakeBody] = queue:to_list(SnakePos),
    D1 = dict:append(SnakeHead, Id, D),
    add_obstacle_point_list_to_map(SnakeBody, D1).

feed_snake(Snake, Foods) ->
    feed_snake(Snake, Foods, []).

feed_snake(Snake, [Food | OtherFoods], DoneFoods) ->
    #snake{position=PosQueue, length=SnakeLength} = Snake,
    #object{position=FoodPos, value=FoodValue} = Food,
    case find_point_in_point_list(front(PosQueue), FoodPos) of
	true ->
	    {fed, {Snake#snake{length=SnakeLength + FoodValue}, DoneFoods ++ OtherFoods}};
	false ->
	    feed_snake(Snake, OtherFoods, [Food | DoneFoods])
    end;
feed_snake(Snake, [], DoneFoods) ->
    {not_fed, {Snake, DoneFoods}}.
    
evaluate_food(Snakes, Foods) ->
    evaluate_food(Snakes, Foods, [], []).

evaluate_food([Snake| OtherSnakes], Foods, NewSnakes, Results) ->
    {Result, {NewSnake, NewFoods}} = feed_snake(Snake, Foods),
    Results1 = case Result of
		   fed ->
		       [{fed, Snake#snake.id} | Results];
		   _Any ->
		       Results
	       end,
    evaluate_food(OtherSnakes, NewFoods, [NewSnake|NewSnakes], Results1);

evaluate_food([], Foods, NewSnakes, Results) ->
    {NewSnakes, Foods, Results}.
	    

process_move(Move, GameState, QueuedMoves) ->
    %% evaluate only the first move, queue the rest
    {move, SnakeId, Direction, Clock} = Move,
    #game_state{snakes=Snakes} = GameState,
    {ok, {S, OtherSnakes}} = find_snake(SnakeId, Snakes),
    case update_snake_direction(Direction, S) of 
	{ok, ChangedSnake} ->
	    NewGameState = GameState#game_state{snakes=[ChangedSnake | OtherSnakes]},
	    {NewGameState, QueuedMoves};
	{ignored, _S} ->
	    NewQueue = queue:in({move, SnakeId, Direction, Clock + 1}, QueuedMoves),
	    {GameState, NewQueue}
    end.


process_queued_moves(GameState, QueuedMoves) ->
    process_queued_moves(GameState, QueuedMoves, queue:new()).

process_queued_moves(GameState, QueuedMoves, OutQueue) ->
    case out(QueuedMoves) of
	{{value, Move}, Q2} ->
	    {NewGameState, NewQueuedMoves} = process_move(Move, GameState, OutQueue),
	    process_queued_moves(NewGameState, Q2, NewQueuedMoves);
	{empty, QueuedMoves} ->
	    {GameState, OutQueue}
    end.

find_snake(SnakeId, Snakes) ->
    find_snake(SnakeId, Snakes, []).

find_snake(SnakeId, [#snake{id=SnakeId} = Snake | After], Before) ->
    {ok, {Snake, Before ++ After}};
find_snake(SnakeId, [H| T], L) ->
    find_snake(SnakeId, T, [H|L]);
find_snake(_, [], L) ->
    {not_found, L}.

update_snake_direction(Direction, #snake{changed=false} = Snake) ->
    {ok, Snake#snake{direction=Direction, changed=true}};
update_snake_direction(_, Snake) ->
    {ignored, Snake}.

move_snakes(#game_state{snakes=Snakes} = GS) ->
    GS#game_state{snakes=map(Snakes, fun move_snake/1)}.

move_snake(#snake{position=Q, direction=D, length=L} = Snake) ->
    Fun = move_snake_function(D),
    Q1 = add_to_front(Fun(front(Q)), Q),
    Snake#snake{position=resize_snake_position(Q1, L), changed = false}.

resize_snake_position(Q, Length) ->
    resize_snake_position(Q, queue:len(Q), Length).

resize_snake_position(Q, QL, L) when QL > L ->
    resize_snake_position(remove_from_back(Q), QL - 1, L);

resize_snake_position(Q, _QL, _L) -> Q.
    

display_board(_,_) ->
    done.

front(Q1) ->
    queue:get(Q1).

add_to_front(Item, Q1) ->
    in_r(Item, Q1).

remove_from_back(Q1) ->
    {_, Q2} = out_r(Q1),
    Q2.

move_snake_function(up) ->
    fun ({X,Y}) ->
	    {X, Y+1}
    end;
move_snake_function(down) ->
    fun ({X,Y}) ->
	    {X, Y-1}
    end;
move_snake_function(left) ->
    fun ({X,Y}) ->
	    {X-1, Y}
    end;
move_snake_function(right) ->
    fun ({X,Y}) ->
	    {X+1, Y}
    end.
