-record(game_state, {snakes, foods, obstacles, size = {20,20}, clock = 0}).

-record(snake, {id, direction, position=queue:new(), length, changed=false}).

-record(object, {type, position, value}).
