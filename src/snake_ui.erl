-module(snake_ui).
-export([init/0]).

%% NOTES:
%%%% Start up
%% When the UI starts up, it will immediately spawn a process before calling gs:start() so that events in the UI are sent to that spawned process
%% The following things will also need to happen.
%% 1. Register the new process as snake_ui
%% 2. Create the canvas and display it
%% 3. Decide which color to use for all 8 snakes on the board as well as the food and obstacles

%% this process that runs the event loop must export the following functions:
%% 1. start/0 to start
%% 2. display_board/1 which takes a list of obstacles and displays them.  The format of the obstacles are defined in game_state.hrl
%% 3. display_food/1 takes a list of food and displays them.
%% 4. display_snakes/1 takes a list of snakes and displays them.

%% the canvas is to be divided into a grid of size (M,N) and each grid cell is to be of size 50x50.  The origin of the grid is at the botton left.
%% the snake is a contiguous filled polygon which is centered in a grid with a 10 px space surrounding it. ie the width of the snake is 30px
%% the snake is rendered by using a list of points that define the grid cells where the snake is currently located 
%%% eg.  a snake with position list [{10,10},{11,10},{12,10},{12,11},{12,12},{12,13},{13,13},{14,13},{15,13}] looks like 
%%          
%%           bbbb
%%           b
%%        Hbbb

%% the first element in the position list is the head of the snake, the body of the snake
%% follows.  A body coordinate always differs from the preceding coordinate in the list by
%% one grid in a single direction (up, down, left or right).

%%%% Snake
%% The snake may be rendered as individual boxes representing each point in the snake
%% position list or the snake position list can be converted into an easier format for
%% drawing a contiguous snake eg. the above snake could be converted into
%% [{{10,10},{12,10}},{{12,10},{12,13}},{{12,13},{15,13}}] to make it easier to draw the
%% snake
%%
%% -note: the snake position list is specified as a queue to access the list in order use
%% queue:to_list/1 in order to extract the list from it.

%%%% Obstacles
%% obstacles will be specified in the same way (as a list of points that the particular
%% obstacle is located). All obstacles will be the same color which is different from the
%% snakes.  Obstacles will have a width of 50px

%%%% Food
%% food will be specified in the same way except the position list for food will contain
%% only one coordinate.  Food will have a width of 30px and will also be a separate color
%% from snakes or obstacles.

%%%% Events
%% When an event occurs, the following will be sent to the registered process game_logic
%% {event, Direction}
%% event is an atom
%% Direction is one of the following atoms <up | down | left | right>

init()->
	S = gs:start(),
	Win = gs:create(window,S,[{width,500},{height,500},{buttonpress,true},{keypress,true}]),
	Can = gs:create(canvas,Win,[{width,400},{height,400},{bg,white},{keypress,true}]),
	Line = gs:create(line,Can,[{coords,[{100,100},{100,200}]},{arrow,none},{width,1}]),

	gs:config(Win,{map,true}),

	loop(Can,Line).

loop(Can,Line)->
	receive
		{gs,_,keypress,Data,[KeySym|_]}->
		io:format("key pressed\n", []),
		%%[{Y,Z}|T] = gs:read(Line,coords),
		%%gs:config(Line,[{coords,[{300,300},{100,300}]},{width,10}]),
		
		%%gs:config(Line,[{move,{300,300},{100,300}}]),
		NewCoords = movesnake(Can,Line,KeySym),
		gs:config(Line,[{coords,NewCoords}]),

		loop(Can,Line)
	end.


movesnake(Can,Line,KeySym)->

	Coords = gs:read(Line, coords),
	movesnake(Can,Line,Coords,KeySym).



movesnake(Can,Line,[{X1,Y1},{X1,Y2}],'Up')->
	
	if
		(X1 =:= 1) ->
			[{X1,Y1},{X1,Y2}];
		(Y1 < Y2) ->
			[{X1,Y1-1},{X1,Y2-1}];
		(Y1 > Y2) ->
			[{X1,Y1},{X1,Y2}]
	end;

		
movesnake(Can,Line,[{X1,Y1},{X2,Y1}],'Up')->
	if
		(X1 =:= 1) ->
			[{X1,Y1},{X2,Y1}];
		((X1+1) < X2) ->
			[{X1,Y1-1},{X1,Y1},{X2-1,Y1}];
		(X1 > (X2+1)) ->
			[{X1,Y1-1},{X1,Y1},{X2+1,Y1}];
		true ->
			[{X1,Y1-1},{X1,Y1}]
	end;


movesnake(Can,Line,[{X1,Y1},{X1,Y2}],'Down')->
	%%CanHeight = gs:read(Can,height),

	if
		(X1 =:= (400 - 1)) ->
			[{x1,Y1},{X1,Y2}]; 
		(Y1<Y2)->
			[{X1,Y1},{X1,Y2}];
		(Y1>Y2)->
			[{X1,Y1+1},{X1,Y2+1}]
	end;


movesnake(Can,Line,[{X1,Y1},{X2,Y1}],'Down')->
	%%CanHeight = gs:read(Can,height),

	if
		(X1 =:= (400 - 1))->		
			[{X1,Y1},{X2,Y1}];
		(X2 > (X1+1)) ->
			[{X1,Y1+1},{X1,Y1},{X2-1,Y1}];
		(X1 > (X2+1)) ->
			[{X1,Y1+1},{X1,Y1},{X2+1,Y1}];
		true ->
			[{X1,Y1+1},{X1,Y1}]
	end;


movesnake(Can,Line,[{X1,Y1},{X2,Y1}],'Left')->
	
	if
		(Y1 =:= 1) ->
			[{X1,Y1},{X2,Y1}];
		(X1 > X2) ->
			[{X1,Y1},{X2,Y1}];
		(X1 < X2) ->
			[{X1-1,Y1},{X2-1,Y1}]
	end;

movesnake(Can,Line,[{X1,Y1},{X1,Y2}],'Left') ->
			
	if
		(Y1 =:= 1) ->
			[{X1,Y1},{X1,Y1}];
		(Y1 > (Y2+1)) ->
			[{X1-1,Y1},{X1,Y1},{X1,Y2+1}];
		(Y2 > (Y1+1)) ->
			[{X1-1,Y1},{X1,Y1},{X1,Y2-1}];
		true ->
			[{X1-1,Y1},{X1,Y1}]
	end;


movesnake(Can,Line,[{X1,Y1},{X2,Y1}],'Right') ->

	%%CanWidth = gs:read(Can,width),
	if
		(Y1 =:= (400 - 1)) ->
			[{X1,Y1},{X2,Y1}];
		(X1 < X2) ->
			[{X1,Y1},{X2,Y1}];
		(X1 > X2) ->
			[{X1+1,Y1},{X2+1,Y1}]
	end;

movesnake(Can,Line,[{X1,Y1},{X1,Y2}],'Right') ->

	%%CanWidth = gs:read(Can,width),
	if
		(Y1 =:= (400 - 1)) ->
			[{X1,Y1},{X1,Y2}];
		(Y1 > (Y2+1)) ->
			[{X1+1,Y1},{X1,Y1},{X1,Y2+1}];
		(Y2 > (Y1+1)) ->
			[{X1+1,Y1},{X1,Y1},{X1,Y2-1}];
		true ->
			[{X1,Y1+1},{X1,Y1}]
	end.
