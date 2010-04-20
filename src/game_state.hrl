%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name: game_state.erl
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
%%% Description: This header file defines the structure of the various records used in the
%%% game.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-record(game_state,
	{
	  %% a list of snakes
	  snakes = [],

	  %% a list of foods.  These are really type object but the object.type will be the atom food 
	  foods = [],

	  %% a list of obstacles.  These are also type object but object.type will be obstacle
	  obstacles = [],

	  %% the size of the grid
	  size = {50,50},

	  %% monotonically increasing clock
	  clock = 0,

	  %% the snakeid of the current node
	  myid,
      
      %% new food that is generated - to be picked up by the clock whenever required
      new_foods = []}).

-record(snake,
	{
	  %% The id of the snake (an atom)
	  id,
	  
	  %% The direction the snake is currently moving in
	  %% can be one of [up , down, left, right]
	  direction,

	  %% the position.  this is different for regular objects in that the position is
	  %% stored as a queue instead of a list.  use queue:to_list/1 to extract the list
	  %% from the queue
	  position=queue:new(),

	  %% the length of the snake
	  length = 0,
	
	  %% current score of the snake
	  score = 0,

	  %% No. of lives granted to each snake
	  lives = 5
	
	}).


%% foods and obstacles are of type object.
-record(object,
	{
	  %% defines the type of the object.
	  %% Possible values are [food, obstacle]
	  type,

	  %% a list of coordinates the define where this object is located
	  %% each coordinate is a tuple consisting of the x,y location 
	  %% e.g., [{10,10},{11,10}]
	  %% the coordinates are in sequence
	  position,

	  %% the value is used for foods to determine something special about the food.
	  %% e.g., regular foods would have a value of one which means any snakes
	  %% consuming this food will grow by one.. special foods might have some other
	  %% value to allow the snakes to grow longer or to make them invisible or
	  %% something.  initially this will be set to just 1
	  value}).


-record(food,
    {
      %% a list of coordinates the define where this object is located
      %% each coordinate is a tuple consisting of the x,y location 
      %% e.g., [{10,10},{11,10}]
      %% the coordinates are in sequence
      position,

      %% the value is used for foods to determine something special about the food.
      %% e.g., regular foods would have a value of one which means any snakes
      %% consuming this food will grow by one.. special foods might have some other
      %% value to allow the snakes to grow longer or to make them invisible or
      %% something.  initially this will be set to just 1
      value,
      
      %% this food item should be alive till logical tick alive_till_tick unless a snake eats it
      alive_till_tick}).
