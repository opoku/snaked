-module(snake_ui).
-export([start/1, display/2, start_gui/1, stop/0, display_obstacles/1]).
-include("game_state.hrl").


start(Coords)->
    Pid = spawn_link(snake_ui, start_gui, [Coords]),
    register(snake_ui, Pid).

stop() ->
    snake_ui ! {die}.

start_gui({X,Y}) ->
    S = gs:start(),
    Win = gs:create(window,S,[{width,750},{height,550},{buttonpress,true},{keypress,true}]),
    Can = gs:create(canvas,Win,[{width, ((X * 10) + 200)},{height, (Y * 10)},{bg,white},{keypress,true}]),
    Snakes_List = [],
    %%Line = gs:create(line,Can,[{coords,[{100,100},{100,110}]},{arrow,none},{width,5}]),
    gs:config(Win,[{map,true},raise]),
    %%display_board([#object{type = obstacle,position = [{25,25},{30,30}] , value = 20}, #object{type = obstacle,position = [{100,100},{105,105}], value = 10}],Can),
    Food_List = [],
    Obstacle_List = [],
    Messages_List = [],
    Colors_List = [{red,none},{blue,none},{green,none},{yellow,none},{orange,none},{pink,none},{purple,none},{brown,none}],
    put(list_Of_Colors, Colors_List),
    %%dont_end().

    loop(Can,Snakes_List,Obstacle_List,Food_List,Messages_List).


handle_keypress(KeySym) ->
    case KeySym of
	D when (D =:= 'Up') or (D =:= 'Down') or (D =:= 'Left') or (D =:= 'Right') ->
	    game_logic:send_event(D);
	_Any ->
	    do_nothing
    end.

loop(Can,Snakes_List,Obstacle_List,Food_List,Messages_List)->
    receive
	{die} ->
	    io:format("UI dying\n"),
	    done;
	{gs,_,keypress,_Data,[KeySym|_]}->
	    io:format("key pressed\n", []),
	    handle_keypress(KeySym),
	    loop(Can,Snakes_List,Obstacle_List,Food_List,Messages_List);

	{display_obstacles, List}->
	    Obstacle_List1= display_board(List,Can),
	    object_disappear(Obstacle_List),
	    io:format("display_board returned\n", []),
	    loop(Can,Snakes_List,Obstacle_List1, Food_List,Messages_List);

	{display_food, List}->
	    Food_List1 = display_food(List, Can),
	    object_disappear(Food_List),
	    loop(Can,Snakes_List,Obstacle_List, Food_List1,Messages_List);

	{display_snakes, List}->
	    io:format("SnakesList ~p~n", [List]),
	    Messages_List1 = display_messages(List,Can), 
	    Snakes_List1 = display_snakes(List, Can),
	    object_disappear(Messages_List),
	    object_disappear(Snakes_List),
	    loop(Can, Snakes_List1, Obstacle_List, Food_List,Messages_List1);

	{add_snake, #snake{id = Id, position = Coords}}->
	    Snakes_List1 = add_snake(Id,Coords,Can,Snakes_List),
	    loop(Can,Snakes_List1, Obstacle_List, Food_List,Messages_List)
    end.

display_obstacles(Obstacles) ->
    snake_ui ! {display_obstacles, Obstacles}.

display(#game_state{snakes = Snakes, obstacles = _Obstacles, foods = Food, size = _Size, clock = _Tick}, _Results) ->
    %% do something with results
    %% Results is a list contains tuples like
    %% {killed, SnakeId} | {eaten, }

    %%io:format("GameState: ~p  Results : ~p~n", [GameState, Results]),

    %%if
	%%Tick < 2 ->
	%%    snake_ui ! {display_obstacles, Obstacles},
	%%true ->
	  %%  done
    %%end,
    snake_ui ! {display_food, Food},
    snake_ui ! {display_snakes, Snakes},
    done.

add_snake(Id, Coords, Can, Snakes_List) ->
    New_coords = resize(Coords),
    Snake = gs:create(line, Can, [{coords,New_coords},{width, 10}]),
    [{Id, Snake}|Snakes_List].

%%get_snake(Id, Snakes_List)->
%%    lists:keyfind(Id,1,Snakes_List).


resize(Coords)->
    lists:map(fun({X,Y})-> {(X * 10), (Y * 10)} end, Coords).

object_disappear(List)->
    lists:foreach(fun(X) -> gs:destroy(X) end, List).


display_board(List, Can)->
    io:format("display board called~n",[]),
    lists:map(fun(X) -> obstacles(X,Can) end, List).


obstacles(#object{type = obstacle, position = Coords},Can)->
    Coords1 = case Coords of
	          [{X,Y}] -> [{X,Y},{X,Y}];
		  _Default -> Coords
	      end, 		
    gs:create(line, Can, [{coords,resize(Coords1)},{fg,black},{width,10}]).

display_food(List,Can)->
    %%io:format("display food called... ~nFoodlist ~p~n", [List]),
    lists:map(fun(X) -> food(X,Can) end, List).

food(#food{position = [{X,Y}]}, Can)->
    [{X1,Y1}] = resize([{X,Y}]),
    gs:create(rectangle, Can, [{coords,[{X1-5,Y1-5},{X1+5,Y1+5}]},{fg,green}, {fill,green}]).

display_snakes(List,Can)->
    lists:delete({false}, lists:map(fun(X)-> snak(X,Can) end, List)).


snak(#snake{position = P,id = Id},Can)->
    Coords = queue:to_list(P),
    Len = length(Coords),
    Colors_List = get(list_Of_Colors),
    Color_Tuple = lists:keysearch(Id, 2, Colors_List),
    case Color_Tuple of
	{value, Snake_Color} -> {Color,_} = Snake_Color,
			New_Colors_List = Colors_List;
	false -> {value,New_Snake_Color} = lists:keysearch(none,2,Colors_List),
			{Color, none} = New_Snake_Color,
		     	New_Colors_List = lists:keyreplace(Color,1,Colors_List,{Color,Id})
	end,
    erase(list_Of_Colors),
    put(list_Of_Colors, New_Colors_List),
    case Len of
	1 -> [{X,Y}] = Coords,
	     Coords1 = [{X,Y},{X,Y}],
	     Ret = gs:create(line, Can, [{coords,resize(Coords1)}, {fg,Color},{width, 10}]);
	0 -> Ret = false;

	_Default-> 
	     Ret = gs:create(line, Can, [{coords,resize(Coords)}, {fg,Color},{width, 10}])
    end,
    Ret.
    

%%display_snakes(List, Can, Snakes_List)->
%%    io:format("display snakes called~n",[]),
%%    lists:map(fun(X) -> snake(X,Can,Snakes_List) end, List).


%%snake(#snake{id = Id, direction = _, position = P, length =_, changed = false},Can,Snakes_List)->;

%%snake(#snake{id = Id, position = P},Can, Snakes_List)->
%%    {Id,Snake} = lists:keyfind(Id,1,Snakes_List),
%%    Coords = resize(P),
%%    gs:config(Snake, Can, [coords,Coords]).

display_messages(List,Can)->	
    %%lists:map(fun(X)-> message(X,Can) end, List).
    display_messages(List,Can,0,[]).

display_messages([H|T],Can,Offset,Results)->
    Coords = {500, 200 + (Offset * 80)},
    Result = message(H,Can,Coords),
    display_messages(T,Can, (Offset+1),Result ++ Results);
     

display_messages([],_,_,Results)->
     Results.


message(#snake{id=Id,score=S,lives=L},Can,{X,Y})->
    Text1 = Id,
    One = gs:create(text,Can,[{coords,[{X,Y}]},{text,Text1},{font,{times,12}},{fg,black}]),
    Text2 = "Score:",
    Two = gs:create(text,Can,[{coords,[{X,Y+20}]},{text,Text2},{font,{times,12}},{fg,black}]),
    Text3 = S,
    Thr = gs:create(text,Can,[{coords,[{X,Y+40}]},{text,Text3},{font,{times,12}},{fg,black}]),
    Text4 = "Lives:",
    Four = gs:create(text,Can,[{coords,[{X,Y+60}]},{text,Text4},{font,{times,12}},{fg,black}]),
    Text5 = L,
    Five = gs:create(text,Can,[{coords,[{X,Y+80}]},{text,Text5},{font,{times,12}},{fg,black}]),
    [One,Two,Thr,Four,Five] .    

