-module(snake_ui).
-export([start/1, display/2, start_gui/1, stop/0]).
-include("game_state.hrl").


start(Coords)->
    Pid = spawn_link(snake_ui, start_gui, [Coords]),
    register(snake_ui, Pid).

stop() ->
    snake_ui ! {die}.

start_gui({X,Y}) ->
    S = gs:start(),
    Win = gs:create(window,S,[{width,1000},{height,1000},{buttonpress,true},{keypress,true}]),
    Can = gs:create(canvas,Win,[{width, (X * 100)},{height, (Y * 100)},{bg,white},{keypress,true}]),
    Snakes_List = [],
    Line = gs:create(line,Can,[{coords,[{100,100},{100,110}]},{arrow,none},{width,5}]),
    gs:config(Win,{map,true}),
    %%display_board([#object{type = obstacle,position = [{25,25},{30,30}] , value = 20}, #object{type = obstacle,position = [{100,100},{105,105}], value = 10}],Can),
    Food_List = [],
    Obstacle_List = [],
    %%dont_end().

    loop(Can,Line,Snakes_List,Obstacle_List,Food_List).


handle_keypress(KeySym) ->
    case KeySym of
	D when (D =:= 'Up') or (D =:= 'Down') or (D =:= 'Left') or (D =:= 'Right') ->
	    game_logic:send_event(D);
	_Any ->
	    do_nothing
    end.

loop(Can,Line,Snakes_List,Obstacle_List,Food_List)->
    receive
	{die} ->
	    io:format("UI dying\n"),
	    done;
	{gs,_,keypress,_Data,[KeySym|_]}->
	    io:format("key pressed\n", []),
	    handle_keypress(KeySym),
	    loop(Can,Line,Snakes_List,Obstacle_List,Food_List);

	{display_obstacles, List}->
	    object_disappear(Obstacle_List),
	    Obstacle_List= display_board(List,Can),
	    loop(Can,Line,Snakes_List,Obstacle_List, Food_List);

	{display_food, List}->
	    object_disappear(Food_List),
	    Food_List = display_food(List, Can),
	    loop(Can, Line,Snakes_List,Obstacle_List, Food_List);

	{display_snakes, List}->
	    object_disappear(Snakes_List),
	    Snakes_List1 = display_snakes(List, Can),
	    loop(Can, Line, Snakes_List1, Obstacle_List, Food_List);

	{add_snake, #snake{id = Id, position = Coords}}->
	    Snakes_List1 = add_snake(Id,Coords,Can,Snakes_List),
	    loop(Can,Line,Snakes_List1, Obstacle_List, Food_List)
    end.


display(#game_state{snakes = Snakes, obstacles = Obstacles, foods = Food, size = Size} = GameState, Results) ->
    %% do something with results
    %% Results is a list contains tuples like
    %% {killed, SnakeId} | {eaten, }

    io:format("GameState: ~p~n", [GameState]),

    snake_ui ! {display_obstacles, Obstacles},
    snake_ui ! {display_food, Food},
    snake_ui ! {display_snakes, Snakes},
    done.


add_snake(Id, Coords, Can, Snakes_List) ->
    New_coords = resize(Coords),
    Snake = gs:create(line, Can, [{coords,New_coords},{width, 5}]),
    [{Id, Snake}|Snakes_List].

get_snake(Id, Snakes_List)->
    lists:keyfind(Id,1,Snakes_List).


resize(Coords)->
    lists:map(fun({X,Y})-> {(X * 5), (Y * 5)} end, Coords).

object_disappear(List)->
    lists:foreach(fun(X) -> gs:destroy(X) end, List).


display_board(List, Can)->
    io:format("display board called~n",[]),
    lists:map(fun(X) -> obstacles(X,Can) end, List).


obstacles(#object{type = obstacle, position = P},Can)->
    gs:create(rectangle, Can, [{coords,P},{fill,cyan}]).

display_food(List,Can)->
    io:format("display food called~n",[]),
    lists:map(fun(X) -> food(X,Can) end, List).

food(#object{type = food, position = P, value = v}, Can)->
    gs:create(rectangle, Can, [{coorgs,P},{fill,green}]).

display_snakes(List,Can)->
    lists:map(fun(X)->snak(X,Can) end, List).

snak(#snake{position = P},Can)->
    gs:create(line, Can, [{coord,queue:to_list(P)},{width, 5}]).
		      


display_snakes(List, Can, Snakes_List)->
    io:format("display snakes called~n",[]),
    lists:map(fun(X) -> snake(X,Can,Snakes_List) end, List).


%%snake(#snake{id = Id, direction = _, position = P, length =_, changed = false},Can,Snakes_List)->;

snake(#snake{id = Id, position = P},Can, Snakes_List)->
    {Id,Snake} = lists:keyfind(Id,1,Snakes_List),
    Coords = resize(P),
    gs:config(Snake, Can, [coords,Coords]).
