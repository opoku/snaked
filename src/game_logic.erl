%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name: game_logic.erl
%%% Author: ROAM DS Group for 2010 18-842 (DS) Class
%%%         Rama (rbupath)
%%%         Aditi (apandya)
%%%         Osei Poku (opoku)
%%%         Manan Patel (mdpatel)
%%%
%%% Date: Spring 2010
%%%
%%% Project Description: This project implements a distributed snake game
%%%
%%% Description: This module implements the game logic of the game.  This involves
%%% evaluating the state of the snakes.  Specifically, the snakes need to move and when
%%% they do, it needs to be determined whether they are still alive or dead.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(game_logic).
-export([start/1, send_event/1]).

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

start(Id) ->
    Pid = spawn(game_logic, init, [Id]),
    register(game_logic, Pid).

stop() ->
    (catch snake_ui:stop()),
    game_logic ! {die}.

init(Id) ->
	
    io:format("Registered ~p as game_logic~n", [self()]),


    Snakes = gen_snakes(),
    Obstacles = gen_obstacles(),

    GameState = #game_state{snakes=Snakes, obstacles=Obstacles, myid = Id},
    %process_flag(trap_exit, true),
    
    snake_ui:start(GameState#game_state.size),
    
    #game_state{snakes=Snakes} = GameState,

    %% there is a queue for each snake
    ReceivedMoveQueue = [{SnakeId, queue:new()} || #snake{id=SnakeId} <- Snakes],
    game_loop(GameState, ReceivedMoveQueue).


gen_snakes() ->
    Pos1 = in({10,10}, queue:new()),
    Pos2 = in({10,9}, Pos1),
    Pos3 = in({10,8}, Pos2),
    
%%     PosB1 = in({100,100}, queue:new()),
%%     PosB2 = in({101,100}, PosB1),
%%     PosB3 = in({102,100}, PosB2),
    Snake1 = #snake{id=one, direction='Down', position=Pos3, length = 3},
%%    Snake2 = #snake{id=two, direction='Left', position=PosB3, length = 3},
    [Snake1].

gen_obstacles() ->
    {Root, _Options} = filename:find_src(game_logic),
    
    PathToBorder = filename:absname_join(filename:dirname(Root), "../resources/border.txt"),
    io:format("path: ~p~n", [PathToBorder]),
    {ok, [Border]} = file:consult(PathToBorder),
    [#object{type=obstacle, position = Border}].

debug() ->
    game_logic ! {self(), get_state},
    Pid = whereis(game_logic),
    receive
	{Pid, Any} ->
	    Any
    end.

get_game_state() ->
    game_logic ! {self(), get_game_state},
    Pid = whereis(game_logic),
    receive
    {Pid, GameState} ->
        GameState
    end.


%%% 
%% possible values for direction are the atoms [up, down, left, right]
%% returns the atom ok
%%%
send_event(Direction) ->
    game_logic ! {event, Direction},
    ok.

game_loop (GameState, ReceivedMoveQueue) ->
    io:format("starting game_loop~n"),
    #game_state{clock=Clock, myid = MyId, foods = Foods} = GameState,
    receive
	{'EXIT', Pid, Reason} ->
	    io:format("Pid ~p exited for reason ~p~n", [Pid, Reason]),
	    game_loop(GameState, ReceivedMoveQueue);
	{print_state} ->
	    io:format("Clock: ~p~nGameState: ~p~nReceivedMoveQueue: ~p~n", [Clock, GameState, ReceivedMoveQueue]),
	    game_loop(GameState, ReceivedMoveQueue);
	{Pid, get_state} ->
	    io:format("Getstate~n"),
	    Pid ! {self(), {GameState, ReceivedMoveQueue}},
	    game_loop(GameState, ReceivedMoveQueue);
	{Pid, get_game_state} ->
	    io:format("Get Game State~n"),
	    Pid ! {self(), GameState},
	    game_loop(GameState, ReceivedMoveQueue);
	{become, Mod, Func} ->
	    io:format("Becoming ~p:~p~n", [Mod, Func]),
	    apply(Mod, Func, [GameState, ReceivedMoveQueue]);
	{die} ->
	    io:format("Game Logic Dying~n");
	{tick, NewClock, NewFoods} ->
	    io:format ("Received tick for clock ~p old Clock ~p~n", [NewClock, Clock]),
	    case Clock + 1 =:= NewClock of
		true ->
		    io:format ("Advancing Clock~n"),
		    MoveEvents = receive_all_events(MyId),
		    %% always broadcast the events even if the movelist is empty
		    message_passer:broadcast(MoveEvents),
		    NewGameState0 = GameState#game_state{foods = Foods ++ NewFoods},
		    {NewGameState, NewReceivedMoveQueue} = advance_game(NewGameState0, ReceivedMoveQueue),
		    %% We create new food here for use by the clock whenever it wants to use.
		    NewGameState1 = food:generate_foods(NewGameState),
		    game_loop(NewGameState1, NewReceivedMoveQueue);

		_Any -> % ignore other 
		    game_loop(GameState, ReceivedMoveQueue)
	    end;
	{move, SnakeId, MoveList} ->
	    %% put this move into the queue for snakeid
	    io:format("Move Called~n"),
	    {SnakeId, Queue} = lists:keyfind(SnakeId, 1, ReceivedMoveQueue),
	    NewQueue = process_move_list(MoveList, Queue),
	    NewReceivedMoveQueue = lists:keystore(SnakeId, 1, ReceivedMoveQueue, {SnakeId, NewQueue}),
	    game_loop(GameState, NewReceivedMoveQueue)
	    end;
	{kill_snake, SnakeId} ->
		%% kill snake => remove snake from all the data structures
		#game_state{snakes = Snakes,_,_,_,_,_,_} = GameState,
		NewSnakes = lists:keydelete(SnakeId, 1, Snakes),
		NewReceivedMoveQueue = 	lists:keydelete(SnakeId, 1, ReceivedMoveQueue),
		game_loop(GameState#game_state{snakes = NewSnakes}, NewReceivedMoveQueue)	
    end.

%% we haven't received the gui events until now. now we just receive all of them an put
%% them in a list in the order they were sent.
receive_all_events(Id) ->
    receive_all_events(Id, []).

receive_all_events(Id, MoveList) ->
    receive
	{event, Direction} ->
	    receive_all_events(Id, [Direction | MoveList])
    after
	0 ->
	    {game_logic, {move, Id, lists:reverse(MoveList)}}
    end.

%% returns {NewGameState, NewMoveQueue}
advance_game(GameState, MoveQueue) ->
    io:format ("Inside advance game~n"),
    {GameState1, MoveQueue1} = move_snakes(GameState,MoveQueue),

    %% Results is basically some messages saying what happened as a result of evaluation
    {GameState2, Results} = evaluate_snakes(GameState1),
    NewMoveQueue = update_move_queue(Results, MoveQueue1),
    NewGameState = advance_clock(GameState2),
    
    %% update the gui
    snake_ui:display(NewGameState, Results),
    io:format ("Done updating display~n"),
    {NewGameState, NewMoveQueue}.

update_move_queue([{killed, SnakeId} | Results], MoveQueue) ->
    NewMoveQueue = lists:keydelete(SnakeId, 1, MoveQueue),
    update_move_queue(Results, NewMoveQueue);
update_move_queue([{_Other, _Id} | Results], MoveQueue) ->
    update_move_queue(Results, MoveQueue);
update_move_queue([], MoveQueue) ->
    MoveQueue.

process_move_list([Direction | OtherMoveList], MoveQueue) ->
    %% put this move into the queue for snakeid
    NewQueue = in(Direction, MoveQueue),
    process_move_list(OtherMoveList, NewQueue);
process_move_list([], MoveQueue) ->
    MoveQueue.

advance_clock(GS) ->
    advance_clock(GS, 1).

advance_clock(#game_state{clock=Clock} = GS, Steps) ->
    GS#game_state{clock=Clock+Steps}.


evaluate_snakes(GS) ->
    %% returns {NewGameState, Results}  where results is the deaths or foods obtained
    %% determine which snakes are dead and which snakes are able to eat any food
    {GS1, Results1} = evaluate_obstacles(GS),
    {GS2, Results2} = evaluate_food(GS1),
    {GS2, Results1 ++ Results2}.


find_point_in_point_list(Point, PointList) ->
    lists:member(Point, PointList).

detect_collision(Snake, ObstacleMap) ->
    %% true or false
    #snake{id=SnakeId, position=SnakePos} = Snake,
    case dict:find(front(SnakePos), ObstacleMap) of
	{ok, [SnakeId]} ->
	    false;
	_Any ->
	    true
    end.


process_dead_snakes(DeadSnakes) ->
	process_dead_snakes(DeadSnakes,[],[],[]).

process_dead_snakes([DeadSnake | OtherDeadSnakes],DeadSnakes1,RegeneratedSnakes,Results)->
    #snake{id=SnakeId, lives=SnakeLives} = DeadSnake,
    case SnakeLives > 0 of
	true ->
	    SnakeLivesLeft = SnakeLives - 1,
	    %%TODO: use a better border generate function%
	    NewPosition = in({1,1},in({2,1},in({3,1},queue:new()))),
	    NewSnake = DeadSnake#snake{lives=SnakeLivesLeft,position=NewPosition,direction='Right',length=3},
	    Results1 = [{regenerated,SnakeId} | Results],
	    RegeneratedSnakes1 = [NewSnake | RegeneratedSnakes],
	    process_dead_snakes(OtherDeadSnakes,DeadSnakes1,RegeneratedSnakes1,Results1);
	false ->
	    Results1 = [{killed,SnakeId} | Results],
	    DeadSnakes2 = [DeadSnake | DeadSnakes1],
	    process_dead_snakes(OtherDeadSnakes,DeadSnakes2,RegeneratedSnakes,Results1)
    end;

process_dead_snakes([],DeadSnakes1,RegeneratedSnakes,Results)->
    {DeadSnakes1,RegeneratedSnakes,Results}.

evaluate_obstacles(GS) ->
    %% returns {GS1, Results}
    #game_state{snakes=Snakes, obstacles=Obs} = GS,
    ObstacleMap = build_obstacle_map(Snakes ++ Obs),
    {DeadSnakes, AliveSnakes} = lists:partition(fun(Snake) -> detect_collision(Snake, ObstacleMap) end, Snakes),
	{_DeadSnakes1,RegeneratedSnakes,Results} = process_dead_snakes(DeadSnakes),
	AliveSnakes1 = AliveSnakes ++ RegeneratedSnakes,
    {GS#game_state{snakes=AliveSnakes1}, Results}.

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
    #snake{position=PosQueue, length=SnakeLength, score=SnakeScore} = Snake,
    #food{position=FoodPos, value=FoodValue} = Food,
    case find_point_in_point_list(front(PosQueue), FoodPos) of
	true ->
	    {fed, {Snake#snake{length=SnakeLength + FoodValue, score=SnakeScore+100}, DoneFoods ++ OtherFoods}};
	false ->
	    feed_snake(Snake, OtherFoods, [Food | DoneFoods])
    end;
feed_snake(Snake, [], DoneFoods) ->
    {not_fed, {Snake, DoneFoods}}.

remove_stale_foods(Foods, Tick) ->
    remove_stale_foods(Foods, [], Tick).
    
remove_stale_foods([Food|RemainingFoods], DoneFoods, Tick) ->
    #food{alive_till_tick = AliveTillTick} = Food,
    case AliveTillTick < Tick of
        false ->
	    remove_stale_foods(RemainingFoods, [Food|DoneFoods], Tick);
        true ->
            remove_stale_foods(RemainingFoods, DoneFoods, Tick)
    end;
remove_stale_foods([], DoneFoods, _Tick) ->
    DoneFoods.

evaluate_food(GS) ->
    %% returns {GS1, Results}
    #game_state{snakes=Snakes, foods=Foods, clock = Tick} = GS,
    {Snakes1, Foods1, Results} = evaluate_food(Snakes, Foods),
    NewFoods = remove_stale_foods(Foods1, Tick),
    {GS#game_state{snakes=Snakes1, foods=NewFoods}, Results}.

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
	    
find_snake(SnakeId, Snakes) ->
    find_snake(SnakeId, Snakes, []).

find_snake(SnakeId, [#snake{id=SnakeId} = Snake | After], Before) ->
    {ok, {Snake, Before ++ After}};
find_snake(SnakeId, [H| T], L) ->
    find_snake(SnakeId, T, [H|L]);
find_snake(_, [], L) ->
    {not_found, L}.

move_snakes(#game_state{snakes=Snakes} = GS, MoveQueue) ->
    {NewSnakes, NewMoveQueue} = move_snakes(Snakes, MoveQueue, []),
    {GS#game_state{snakes=NewSnakes}, NewMoveQueue}.

move_snakes([#snake{id=SnakeId, direction=D} = Snake | OtherSnakes], MoveQueue, DoneSnakes) ->
    {SnakeId, Queue} = lists:keyfind(SnakeId, 1, MoveQueue),
    case out(Queue) of
	{{value, Dir}, NewQueue} ->
	    NewSnake = move_snake(Snake, Dir),
	    NewMoveQueue = lists:keystore(SnakeId, 1, MoveQueue, {SnakeId, NewQueue}),
	    move_snakes(OtherSnakes, NewMoveQueue, [NewSnake| DoneSnakes]);
	{empty, Queue} ->
	    NewSnake = move_snake(Snake, D),
	    move_snakes(OtherSnakes, MoveQueue, [NewSnake | DoneSnakes])
    end;

move_snakes([], MoveQueue, DoneSnakes) ->
    {DoneSnakes, MoveQueue}.

move_snake(#snake{position=Q, length=L} = Snake, D) ->
    Fun = move_snake_function(D),
    Q1 = add_to_front(Fun(front(Q)), Q),
    Snake#snake{position=resize_snake_position(Q1, L), direction=D}.

resize_snake_position(Q, Length) ->
    resize_snake_position(Q, queue:len(Q), Length).

resize_snake_position(Q, QL, L) when QL > L ->
    resize_snake_position(remove_from_back(Q), QL - 1, L);

resize_snake_position(Q, _QL, _L) -> Q.
    

%% display_board(GameState,Results) ->
%%     io:format("GS:~p~n Results:~p~n", [GameState, Results]).

front(Q1) ->
    queue:get(Q1).

add_to_front(Item, Q1) ->
    in_r(Item, Q1).

remove_from_back(Q1) ->
    {_, Q2} = out_r(Q1),
    Q2.

move_snake_function('Down') ->
    fun ({X,Y}) ->
	    {X, Y+1}
    end;
move_snake_function('Up') ->
    fun ({X,Y}) ->
	    {X, Y-1}
    end;
move_snake_function('Left') ->
    fun ({X,Y}) ->
	    {X-1, Y}
    end;
move_snake_function('Right') ->
    fun ({X,Y}) ->
	    {X+1, Y}
    end.
