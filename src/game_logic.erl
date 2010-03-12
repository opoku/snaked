-module(game_logic).
-compile([export_all]).

-import(queue, [out/1, in/2]).

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

-record(game_state, {snakes, foods, obstacles, size = {20,20}}).
-record(snake, {id, direction, position, clock}).
-record(object, {type, position}).

print_game_state(GameState).

process_queued_moves(Clock, GameState, QueuedMoves) ->
    process_queued_moves(Clock, GameState, QueuedMoves, queue:new()).

process_queued_moves(Clock, GameState, QueuedMoves, OutQueue) ->
    case out(QueuedMoves) of
	{{value, {move, SnakeID, Direction, Clock}}, Q2} ->
	    case update_snake_direction(Clock, SnakeId, Direction, GameState) of %% 
		{moved, NewGameState} ->
		    process_queued_moves(Clock, NewGameState, Q2, OutQueue);
		ignored ->
		    NewQueue = in({move, SnakeId, Direction, Clock + 1}, OutQueue),
		    process_queued_moves(Clock, GameState, Q2, NewQueue)
	    end;
	{empty, QueuedMoves} ->
	    OutQueue.

%% returns {moved, NewGameState} or ignored
update_snake_direction(Clock, SnakeId, Direction, #game_state{snakes=Snakes} = GameState) ->
    case update_snake_direction(SnakeId, Snakes) of
	{moved, NewSnakes} ->
	    {moved, GameState#game_state{snakes=NewSnakes}};
	ignored ->
    

game_loop(GameState) ->
    game_loop(0, GameState, queue:new()).

game_loop (Clock, GameState, QueuedMoves) ->
    if
	queue:is_empty(QueuedMoves) ->
	    receive
		{tick, Clock + 1} ->
		    %% update the gui
		    NewGameState = evaluate_snake(GameState),
		    display_board(NewGameState),
		    game_loop(Clock + 1, NewGameState, QueuedMoves);
		{move, SnakeId, Direction, Clock} ->
		    %% evaluate only the first move, queue the rest
		    case update_snake_direction(Clock, SnakeId, Direction, GameState) of %% 
			{moved, NewGameState} ->
			    game_loop(Clock, NewGameState, QueuedMoves);
			ignored ->
			    NewQueue = queue:in({move, SnakeId, Direction, Clock + 1}, QueuedMoves),
			    game_loop(Clock, BoardState, SnakeState, NewQueue)
		    end
	    end;
	true -> %% QueuedMoves is not empty
	    NewQueue = process_queued_moves(Clock, GameState, QueuedMoves),
	    game_loop(Clock, GameState, NewQueue)
    end.
