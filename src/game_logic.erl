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
-export([start/2, send_event/1]).

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

start(Id, GameInfo) ->
    Pid = spawn(game_logic, init, [Id, GameInfo]),
    register(game_logic, Pid).

stop() ->
    (catch snake_ui:stop()),
    game_logic ! {die}.

init(Id, GameInfo) ->

    {_,_,NodeList} = GameInfo,
	
    %%Snakes = gen_snakes(),
    Obstacles = gen_obstacles(),
    Snakes = [#snake{id=NodeId} || NodeId <- NodeList],

    GameState = #game_state{obstacles=Obstacles, myid = Id, snakes=Snakes},
    %process_flag(trap_exit, true),
    put(events, []),
    put(unseen_nodes, NodeList),
    put(ticks, []),

    snake_ui:start(GameState#game_state.size),
    
    io:format("DEBUG: after ui start~n"),
    
    %% there is a queue for each snake
    ReceivedMoveQueue = [{NodeId, queue:new()} || NodeId <- NodeList ],
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

get_new_player_position() ->
    game_logic ! {self(), get_new_player_position},
    Pid = whereis(game_logic),
    receive
    {Pid, Pos} ->
        Pos
    end.

update_game_state(GameState) ->
    game_logic ! {game_state, GameState},
    ok.

%%% 
%% possible values for direction are the atoms [up, down, left, right]
%% returns the atom ok
%%%
send_event(Direction) ->
    game_logic ! {event, Direction},
    ok.

%% TODO: game logic needs to start receiving the events and then notify the game manager
%% when it has received all expected events.

start_game() ->
    game_logic ! {start_game},
    ok.

game_loop(#game_state{state=new, myid=MyId}=GameState, RMQ) ->
    %%io:format("DEBUG: new game loop, received game state --> ~p~n", [GameState]),
    io:format("DEBUG: new game loop~n"),
    receive
	{tick, NewClock, _Options} = Msg ->
	    put(ticks, [Msg | get(ticks)]),
	    message_passer:broadcast({game_logic, {move, MyId, NewClock-1, []}}),
	    game_loop(GameState, RMQ);
	{move, SnakeId, _EventClock, _MoveList} = Msg ->
	    put(events, [Msg|get(events)]),
	    Unseen = get(unseen_nodes) -- [SnakeId],
	    put(unseen_nodes, Unseen),
	    case Unseen of
		[] ->
		    %% i have seen all the nodes
		    %% so tell the game manager
		    io:format("I have seen all the nodes so Im telling the game manager~n"),
		    game_manager ! {started, game_logic},
		    receive
			{game_state, #game_state{clock=Clock} = NewGameState} ->
			    io:format("I have received the game state~n"),
			    %% erase(Key) returns value and then erases the key
			    %% keep all the ticks that are after the clock in
			    %% game state
			    Ticks = [Tick || {tick, NewClock, _} = Tick <- lists:reverse(erase(ticks)), NewClock > Clock ],

			    %% keep all events that are for the current clock and after
			    Events = [Event || {move, _, EventClock1, _} = Event <- lists:reverse(erase(events)), EventClock1 >= Clock ],

			    lists:foreach(fun(M) -> self() ! M end, Ticks ++ Events),

			    %% clears the last element in process dictionary
			    erase(unseen_nodes),
			    game_loop(NewGameState#game_state{myid=MyId}, RMQ)
		    end;
		_Any ->
		    game_loop(GameState, RMQ)		    
	    end;
	{start_game} ->
	    game_loop(GameState#game_state{state=started}, RMQ)
    end;

game_loop (#game_state{state=started} = GameState, ReceivedMoveQueue) ->
    #game_state{clock=Clock, myid = MyId} = GameState,
    io:format("DEBUG: started game loop ~p~n", [Clock]),
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
	{Pid, get_new_player_position} ->
	    io:format("Get New Player Position~n"),
	    #game_state{new_player_positions=NewPos} = GameState,
	    Pid ! {self(), NewPos},
	    game_loop(GameState#game_state{new_player_positions=[]}, ReceivedMoveQueue);
	{Pid, get_game_state} ->
	    io:format("Get Game State~n"),
	    Pid ! {self(), GameState},
	    game_loop(GameState, ReceivedMoveQueue);
	{become, Mod, Func} ->
	    io:format("Becoming ~p:~p~n", [Mod, Func]),
	    apply(Mod, Func, [GameState, ReceivedMoveQueue]);
	{die} ->
	    io:format("Game Logic Dying~n");
	{add_player, NodeId} ->
	    io:format("Adding player ~p~n", [NodeId]),
	    #game_state{snakes=Snakes} = GameState,
	    Snakes1 = [#snake{id=NodeId} |Snakes],
	    NewReceivedMoveQueue = [{NodeId, queue:new()} | ReceivedMoveQueue],
	    game_loop(GameState#game_state{snakes=Snakes1}, NewReceivedMoveQueue);
	{tick, NewClock, Options} ->
	    io:format ("Received tick for clock ~p old Clock ~p~n", [NewClock, Clock]),
	    case Clock + 1 =:= NewClock of
		true ->
		    io:format ("Advancing Clock~n"),
		    MoveEvents = receive_all_events(),
		    MoveMsg = {game_logic, {move, MyId, NewClock, MoveEvents}},

		    %% always broadcast the events even if the movelist is empty
		    message_passer:broadcast(MoveMsg),
		    
		    %% two possisble options so far: new food and new player positions.
		    %% They are indexed in the options list by the atoms food and newpos
		    NewGameState0 = process_options(GameState, Options),

		    {NewGameState, NewReceivedMoveQueue} = advance_game(NewGameState0, ReceivedMoveQueue),
		    %% We create new food here for use by the clock whenever it wants to use.

		    NewGameState1 = case game_manager:is_leader() of
					true ->
					    food:generate_foods(NewGameState);
					false ->
					    NewGameState
				    end,
		    game_loop(NewGameState1, NewReceivedMoveQueue);

		_Any -> % ignore other 
		    game_loop(GameState, ReceivedMoveQueue)
	    end;
	%% this will only match those events that are for the current clock
	{move, _SnakeId, Clock, []} ->
	    io:format("move, empty move list~n"),
	    %% an empty movelist should be ignored
	    game_loop(GameState, ReceivedMoveQueue);
	{move, SnakeId, Clock, MoveList} -> 
	    io:format("move, movelist--> ~p~n", [MoveList]),
	    %% put this move into the queue for snakeid
	    io:format("Snake ~p Move event received: ~p~n", [SnakeId, MoveList]),
	    Snakes = GameState#game_state.snakes,
	    case {game_manager:is_leader(), lists:keyfind(SnakeId, #snake.id, Snakes)} of
		{true, #snake{length=0}} ->
		    %% zero length snake and i am the leader
		    NewSnakePosList = GameState#game_state.new_player_positions,
		    NewSnakePosList1 = [generate_new_snake_position(SnakeId, length(NewSnakePosList)) | NewSnakePosList],
		    game_loop(GameState#game_state{new_player_positions=NewSnakePosList1}, ReceivedMoveQueue);
		{false, #snake{length=0}} ->
		    %% ignore this move
		    game_loop(GameState, ReceivedMoveQueue);
		{_, #snake{length=L}} when L > 0 ->
		    %% do nothing
		    {SnakeId, Queue} = lists:keyfind(SnakeId, 1, ReceivedMoveQueue),
		    %% attach the clock value to the move when inserting in queue
		    NewQueue = process_move_list([{Clock,Move} || Move <- MoveList], Queue),
		    NewReceivedMoveQueue = lists:keystore(SnakeId, 1, ReceivedMoveQueue, {SnakeId, NewQueue}),
		    game_loop(GameState, NewReceivedMoveQueue);
		{_, false} ->
		    %% shouldnt happen
		    io:format("Error: received a move event from an unregistered snake ~p~n", [SnakeId]),
		    %% do nothing
		    game_loop(GameState, ReceivedMoveQueue)
	    end
    end.

generate_new_snake_position(SnakeId, NumNewPlayers) ->
    case NumNewPlayers rem 2 of
	0 -> % even
	    {SnakeId, [{3,1},{2,1},{1,1}], 'Right'};
	1 -> % odd
	    {SnakeId, [{1,5},{1,4},{1,3}], 'Down'}
    end.

process_options(GameState, [{food, NewFoods}|Rest]) ->
    #game_state{foods = Foods} = GameState,
    process_options(GameState#game_state{foods = Foods ++ NewFoods}, Rest);
process_options(GameState, [{newpos, SnakePosList} | Rest]) ->
    %% SnakePosList is a list of {SnakeId, Position} where Position is a list of {x,y}
    %% coordinates
    #game_state{snakes=Snakes} = GameState,
    %% update snake position for each new snake
    NewSnakes = update_new_snake_position(Snakes, SnakePosList),
    process_options(GameState#game_state{snakes=NewSnakes}, Rest);
process_options(GameState, []) ->
    GameState.

update_new_snake_position(Snakes, SnakePositionList) ->
    update_new_snake_position(Snakes, SnakePositionList, []).

update_new_snake_position(Snakes, [{SnakeId, Position, Dir} | Rest], Done) ->
    case lists:keytake(SnakeId, #snake.id, Snakes) of
	false ->
	    update_new_snake_position(Snakes, Rest, Done);
	{value, Snake, OtherSnakes} ->
	    Length = length(Position),
	    update_new_snake_position(OtherSnakes, Rest, [Snake#snake{position=queue:from_list(Position), length=Length, direction=Dir} | Done])
    end;
update_new_snake_position(Snakes, [], Done) ->
    Snakes ++ Done.

%% we haven't received the gui events until now. now we just receive all of them an put
%% them in a list in the order they were sent.
receive_all_events() ->
    receive_all_events([]).

receive_all_events(MoveList) ->
    receive
	{event, Direction} ->
	    receive_all_events([Direction | MoveList])
    after
	0 ->
	     lists:reverse(MoveList)
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

detect_collision(#snake{position={[],[]}}, _ObstacleMap) ->
    false;

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
    case queue:to_list(SnakePos) of
	[SnakeHead | SnakeBody] ->
	    D1 = dict:append(SnakeHead, Id, D),
	    add_obstacle_point_list_to_map(SnakeBody, D1);
	[] ->
	    D
    end.

feed_snake(#snake{position={[],[]}}=Snake, Foods) ->
    {not_fed, {Snake, Foods}};
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
    case {D, out(Queue)} of
	{undefined, _} ->
	    move_snakes(OtherSnakes, MoveQueue, [Snake | DoneSnakes]);
	{_, {{value, {_, Dir}}, NewQueue}} ->
	    NewSnake = move_snake(Snake, Dir),
	    NewMoveQueue = lists:keystore(SnakeId, 1, MoveQueue, {SnakeId, NewQueue}),
	    move_snakes(OtherSnakes, NewMoveQueue, [NewSnake| DoneSnakes]);
	{_, {empty, Queue}} ->
	    NewSnake = move_snake(Snake, D),
	    move_snakes(OtherSnakes, MoveQueue, [NewSnake | DoneSnakes])
    end;

move_snakes([], MoveQueue, DoneSnakes) ->
    {DoneSnakes, MoveQueue}.

%% ignore directions in the opposite direction
move_snake(#snake{direction='Down'} = Snake, 'Up')->
    Snake;
move_snake(#snake{direction='Up'} = Snake, 'Down')->
    Snake;
move_snake(#snake{direction='Left'} = Snake, 'Right')->
    Snake;
move_snake(#snake{direction='Right'} = Snake, 'Left')->
    Snake;
%% actually move the snake
move_snake(#snake{position={[],[]}}=Snake, _D) ->
    Snake;
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
