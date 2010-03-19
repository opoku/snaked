-module(snake_ui).
-export([start/1]).
-include("game_state.hrl").


start(Coords)->
	Gui_pid = spawn(fun() -> start_gui(Coords) end),
	register(snake_ui, Gui_pid).

start_gui([X,Y]) ->
	S = gs:start(),
	Win = gs:create(window,S,[{width,1000},{height,1000},{buttonpress,true},{keypress,true}]),
	Can = gs:create(canvas,Win,[{width, (X * 100)},{height, (Y * 100)},{bg,white},{keypress,true}]),
	Snakes_list = [],
	Line = gs:create(line,Can,[{coords,[{100,100},{100,110}]},{arrow,none},{width,5}]),
	gs:config(Win,{map,true}),
	display_board([#object{type = obstacle,position = [{25,25},{30,30}] , value = 20}, #object{type = obstacle,position = [{100,100},{105,105}], value = 10}],Can),
	Food_List = [],
	Obtsacle_List = [],
	%%dont_end().

	loop(Can,Line,Snakes_List,Obstacle_List,Food_List).


dont_end()->
	dont_end().

loop(Can,Line,Snakes_List,Obstacle_List,Food_List)->
	receive
		{gs,_,keypress,Data,[KeySym|_]}->
			io:format("key pressed\n", []),
			game_logic ! {snake_ui, keypress, KeySym},
			loop(Can,Line,Snakes_List,Obstacle_List,Food_List);

		{_,display_obstacles, List}->
			object_disappear(Obstacle_List, Can),
			Obstacle_List= display_board(List,Can),
			loop(Can,Line,Snakes_List,Obstacle_List, Food_List);

		{_,display_food, List}->
			object_disappear(Food_List, Can),
			Food_List = display_food(List, Can),
			loop(Can, Line,Snakes_List,Obstacle_List, Food_List);

		{_,display_snakes, List}->
			display_snakes(List, Can, Snakes_List),
			loop(Can, Line, Snakes_List, Obstacle_List, Food_List);

		{_, add_snake, #snake{id = Id, direction = _, position = Coords, length = _}}->
			Snakes_List = add_snake(Id,Coords,Can,Snakes_List),
			loop(Can,Line,Snakes_List, Obstacle_List, Food_List)
	end.


add_snake(Id, Coords, Can, Snakes_List) ->
	New_coords = resize(Coords),
	Snake = gs:create(line, Can, [{coords,New_coords},{width, 5}]),
	[{Id, Snake}|Snakes_List].

get_snake(Id, Snakes_List)->
	lists:keyfind(Id,1,Snakes_List).


resize(Coords)->
	lists:map(fun({X,Y})-> {(X * 5), (Y * 5)} end, Coords).

object_disappear(List,Can)->
		lists:map(fun(X) -> object_invisible(X,Can) end, List).

object_invisible(X,Can)->
		gs:config(X, Can, [{color, white}]).


display_board(List, Can)->
		io:format("displlay board called\n",[]),
		Result = [],
		Result = lists:map(fun(X) -> obstacles(X,Can) end, List),
		Result.


obstacles(#object{type = obstacle, position = P, value = V},Can)->
		gs:create(rectangle, Can, [{coords,P},{fill,cyan}]).

display_food(List,Can)->
		io:format("display food called",[]),
		lists:map(fun(X) -> food(X,Can) end, List).

food(#object{type = food, position = P, value = v}, Can)->
		gs:create(rectangle, Can, [{coorgs,P},{fill,green}]).


display_snakes(List, Can, Snakes_List)->
		io:format("display snakes called",[]),
		lists:map(fun(X) -> snake(X,Can,Snakes_List) end, List).


%%snake(#snake{id = Id, direction = _, position = P, length =_, changed = false},Can,Snakes_List)->;
		
snake(#snake{id = Id, direction = _, position = P, length =_, changed = true},Can, Snakes_List)->
	{_,Snake} = lists:keyfind(Id,1,Snakes_List),
	Coords = resize(P),
	gs:config(Snake, [coords,Coords]).
	
























































		