-record(game_state, {snakes = [], foods = [], obstacles = [], size = {20,20}, clock = 0, myid}).

-record(snake, {id, direction, position=queue:new(), length = 1}).


%% foods and obstacles are of type object.
-record(object, {type, position, value}).

