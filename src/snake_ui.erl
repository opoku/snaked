-module(snake_ui).
-export([init/0]).

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

























































		
