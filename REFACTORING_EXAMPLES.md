# Concrete Refactoring Examples for Snake Game

This document provides specific, actionable refactoring examples that can be applied to improve the codebase.

## Example 1: Refactoring `game_loop/2` Message Handlers

### Before (Lines 254-288 in game_logic.erl)
```erlang
{move, SnakeId, Clock, MoveList} -> 
    ?LOG("move, movelist--> ~p~n", [MoveList]),
    %% put this move into the queue for snakeid
    ?LOG("Snake ~p Move event received: ~p~n", [SnakeId, MoveList]),
    put(expected_events, get(expected_events) -- [SnakeId]),
    Snakes = GameState#game_state.snakes,
    case {game_manager:is_leader(), lists:keyfind(SnakeId, #snake.id, Snakes)} of
        {true, #snake{length=0}} ->
            %% zero length snake and i am the leader
            NewSnakePosList = GameState#game_state.new_player_positions,
            GridSize = GameState#game_state.size,
            NewSnakePosList1 = [generate_new_snake_position(SnakeId, length(NewSnakePosList), GridSize) | NewSnakePosList],
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
            ?LOG("Error: received a move event from an unregistered snake ~p~n", [SnakeId]),
            %% do nothing
            game_loop(GameState, ReceivedMoveQueue)
    end;
```

### After (Refactored)
```erlang
%% In game_loop/2 receive block:
{move, SnakeId, Clock, MoveList} -> 
    handle_move_event(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue);

%% New extracted functions:
handle_move_event(SnakeId, Clock, [], GameState, ReceivedMoveQueue) ->
    %% Empty move list should be ignored
    ?LOG("move, empty move list~n", []),
    remove_from_expected_events(SnakeId),
    game_loop(GameState, ReceivedMoveQueue);
    
handle_move_event(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue) ->
    ?LOG("Snake ~p Move event received: ~p~n", [SnakeId, MoveList]),
    remove_from_expected_events(SnakeId),
    
    Snakes = GameState#game_state.snakes,
    Snake = lists:keyfind(SnakeId, #snake.id, Snakes),
    IsLeader = game_manager:is_leader(),
    
    case classify_move_event(Snake, IsLeader) of
        new_player_as_leader ->
            handle_new_player_move_as_leader(SnakeId, GameState, ReceivedMoveQueue);
        new_player_as_follower ->
            handle_new_player_move_as_follower(GameState, ReceivedMoveQueue);
        existing_player ->
            handle_existing_player_move(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue);
        unregistered_snake ->
            handle_unregistered_snake_move(SnakeId, GameState, ReceivedMoveQueue)
    end.

classify_move_event(false, _IsLeader) -> unregistered_snake;
classify_move_event(#snake{length=0}, true) -> new_player_as_leader;
classify_move_event(#snake{length=0}, false) -> new_player_as_follower;
classify_move_event(#snake{length=L}, _IsLeader) when L > 0 -> existing_player.

handle_new_player_move_as_leader(SnakeId, GameState, ReceivedMoveQueue) ->
    NewSnakePosList = GameState#game_state.new_player_positions,
    GridSize = GameState#game_state.size,
    NumExistingNewPlayers = length(NewSnakePosList),
    NewPosition = generate_new_snake_position(SnakeId, NumExistingNewPlayers, GridSize),
    UpdatedPositionsList = [NewPosition | NewSnakePosList],
    UpdatedGameState = GameState#game_state{new_player_positions = UpdatedPositionsList},
    game_loop(UpdatedGameState, ReceivedMoveQueue).

handle_new_player_move_as_follower(GameState, ReceivedMoveQueue) ->
    %% Followers ignore moves from new players (leaders handle it)
    game_loop(GameState, ReceivedMoveQueue).

handle_existing_player_move(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue) ->
    {SnakeId, Queue} = lists:keyfind(SnakeId, 1, ReceivedMoveQueue),
    TimestampedMoves = [{Clock, Move} || Move <- MoveList],
    UpdatedQueue = process_move_list(TimestampedMoves, Queue),
    UpdatedMoveQueue = lists:keystore(SnakeId, 1, ReceivedMoveQueue, {SnakeId, UpdatedQueue}),
    game_loop(GameState, UpdatedMoveQueue).

handle_unregistered_snake_move(SnakeId, GameState, ReceivedMoveQueue) ->
    ?LOG("Error: received a move event from an unregistered snake ~p~n", [SnakeId]),
    game_loop(GameState, ReceivedMoveQueue).

remove_from_expected_events(SnakeId) ->
    ExpectedEvents = get(expected_events),
    UpdatedEvents = ExpectedEvents -- [SnakeId],
    put(expected_events, UpdatedEvents).
```

**Benefits:**
- Each function has a single, clear purpose
- Logic is easier to test in isolation
- Names describe what the code does
- Reduced nesting depth from 4-5 levels to 2-3 levels
- Pattern matching is clearer with the classify function

---

## Example 2: Extracting Constants

### Create New File: `src/game_config.hrl`
```erlang
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name: game_config.hrl
%%% Description: Configuration constants for the distributed snake game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Player and Game Limits
-define(MAX_PLAYERS, 8).
-define(DEFAULT_SNAKE_LIVES, 3).
-define(STARTING_SNAKE_LENGTH, 3).

%% Timing Constants (in milliseconds)
-define(TICK_RETRY_DELAY_MS, 50).
-define(MAX_TICK_RETRIES, 50).
-define(DEFAULT_MANAGER_TIMEOUT_MS, 5000).
-define(TCP_SEND_TIMEOUT_MS, 3000).
-define(JOIN_TIMEOUT_MS, infinity).  % Was hardcoded as 'infinity' in line 228

%% Scoring
-define(FOOD_SCORE_VALUE, 100).
-define(FOOD_GROWTH_VALUE, 1).  % How much snake grows when eating food

%% UI Constants
-define(GRID_CELL_SIZE_PX, 10).
-define(WINDOW_WIDTH_PX, 750).
-define(WINDOW_HEIGHT_PX, 550).
-define(SNAKE_LINE_WIDTH_PX, 10).
-define(SCORE_DISPLAY_X_OFFSET, 500).
-define(SCORE_DISPLAY_Y_BASE, 200).
-define(SCORE_DISPLAY_Y_SPACING, 80).

%% Default Grid Size
-define(DEFAULT_GRID_WIDTH, 50).
-define(DEFAULT_GRID_HEIGHT, 40).

%% Snake Starting Positions (number of positions to cycle through)
-define(NUM_STARTING_POSITIONS, 8).
```

### Updated Usage Example (game_logic.erl)
```erlang
% Before (line 601):
{fed, {Snake#snake{length=SnakeLength + FoodValue, score=SnakeScore+100}, DoneFoods ++ OtherFoods}};

% After:
-include("game_config.hrl").
{fed, {Snake#snake{length=SnakeLength + FoodValue, 
                   score=SnakeScore + ?FOOD_SCORE_VALUE}, 
       DoneFoods ++ OtherFoods}};
```

### Updated Usage Example (game_manager.erl)
```erlang
% Before (line 5):
-define(MAX_PLAYERS, 8).

% After:
-include("game_config.hrl").
% Remove the local definition, use the one from config
```

---

## Example 3: Improving Variable Names

### Before (game_logic.erl, lines 688-691)
```erlang
move_snake(#snake{position=Q, length=L} = Snake, D) ->
    Fun = move_snake_function(D),
    Q1 = add_to_front(Fun(front(Q)), Q),
    Snake#snake{position=resize_snake_position(Q1, L), direction=D}.
```

### After
```erlang
move_snake(#snake{position=PositionQueue, length=SnakeLength} = Snake, Direction) ->
    MovementFunction = move_snake_function(Direction),
    CurrentHeadPosition = front(PositionQueue),
    NewHeadPosition = MovementFunction(CurrentHeadPosition),
    ExtendedQueue = add_to_front(NewHeadPosition, PositionQueue),
    TrimmedQueue = resize_snake_position(ExtendedQueue, SnakeLength),
    Snake#snake{position=TrimmedQueue, direction=Direction}.
```

**Benefits:**
- Immediately clear what each variable represents
- Self-documenting code
- Easier for new developers to understand

---

## Example 4: Simplifying Nested Conditionals in UI

### Before (snake_ui.erl, lines 143-166)
```erlang
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
```

### After
```erlang
-include("game_config.hrl").

snak(#snake{position=PositionQueue, id=SnakeId}, Canvas) ->
    Coordinates = queue:to_list(PositionQueue),
    Color = get_or_assign_snake_color(SnakeId),
    create_snake_visual(Coordinates, Color, Canvas).

get_or_assign_snake_color(SnakeId) ->
    ColorsList = get(list_Of_Colors),
    case find_snake_color(SnakeId, ColorsList) of
        {found, Color} ->
            Color;
        not_found ->
            assign_new_color_to_snake(SnakeId, ColorsList)
    end.

find_snake_color(SnakeId, ColorsList) ->
    case lists:keysearch(SnakeId, 2, ColorsList) of
        {value, {Color, SnakeId}} -> {found, Color};
        false -> not_found
    end.

assign_new_color_to_snake(SnakeId, ColorsList) ->
    {value, {AvailableColor, none}} = lists:keysearch(none, 2, ColorsList),
    UpdatedColorsList = lists:keyreplace(AvailableColor, 1, ColorsList, 
                                        {AvailableColor, SnakeId}),
    put(list_Of_Colors, UpdatedColorsList),
    AvailableColor.

create_snake_visual([], _Color, _Canvas) ->
    false;  % Empty snake, no visual
    
create_snake_visual([{X, Y}], Color, Canvas) ->
    % Single coordinate needs duplication for line drawing
    DuplicatedCoords = [{X, Y}, {X, Y}],
    ResizedCoords = resize(DuplicatedCoords),
    gs:create(line, Canvas, [{coords, ResizedCoords}, 
                             {fg, Color}, 
                             {width, ?SNAKE_LINE_WIDTH_PX}]);
    
create_snake_visual(Coordinates, Color, Canvas) when length(Coordinates) > 1 ->
    ResizedCoords = resize(Coordinates),
    gs:create(line, Canvas, [{coords, ResizedCoords}, 
                             {fg, Color}, 
                             {width, ?SNAKE_LINE_WIDTH_PX}]).
```

**Benefits:**
- Separated color management from visual rendering
- Each function has one clear responsibility
- Nesting reduced from 3-4 levels to 1-2 levels
- More testable (can test color assignment independently)

---

## Example 5: Simplifying Position Generation with Data Structure

### Before (game_logic.erl, lines 388-407)
```erlang
generate_new_snake_position(SnakeId, NumNewPlayers, {GridX, GridY}) ->
    case NumNewPlayers rem 8 of
        0 -> % even
            {SnakeId, [{3,1},{2,1},{1,1}], 'Right'};
        1 -> % odd
            {SnakeId, [{1,5},{1,4},{1,3}], 'Down'};
        2 -> % even
            {SnakeId, [{1, GridY - 2},{1, GridY - 3},{1, GridY - 4}], 'Up'};
        3 -> % odd
            {SnakeId, [{5, GridY - 2},{4, GridY - 2},{3, GridY - 2}], 'Right'};
        4 -> % even
            {SnakeId, [{GridX - 4, GridY - 2},{GridX - 3, GridY - 2},{GridX - 2, GridY - 2}], 'Left'};
        5 -> % odd
            {SnakeId, [{GridX - 2, GridY - 6},{GridX - 2, GridY - 5},{GridX - 2, GridY - 4}], 'Up'};
        6 -> % even
            {SnakeId, [{GridX - 2, 5},{GridX - 2, 4},{GridX - 2, 3}], 'Down'};
        7 -> % odd
            {SnakeId, [{GridX - 6, 1},{GridX - 5, 1},{GridX - 4, 1}], 'Left'}
    end.
```

### After
```erlang
-include("game_config.hrl").

%% Starting positions arranged around the perimeter of the grid
%% Format: {PositionIndex, PositionGeneratorFunction, InitialDirection}
get_starting_position_config() ->
    [
        {0, fun(_GX, _GY) -> [{3,1}, {2,1}, {1,1}] end, 'Right'},  % Top-left, moving right
        {1, fun(_GX, _GY) -> [{1,5}, {1,4}, {1,3}] end, 'Down'},   % Left side, moving down
        {2, fun(_GX, GY) -> [{1,GY-2}, {1,GY-3}, {1,GY-4}] end, 'Up'},  % Bottom-left, moving up
        {3, fun(_GX, GY) -> [{5,GY-2}, {4,GY-2}, {3,GY-2}] end, 'Right'},  % Bottom, moving right
        {4, fun(GX, GY) -> [{GX-4,GY-2}, {GX-3,GY-2}, {GX-2,GY-2}] end, 'Left'},  % Bottom-right, moving left
        {5, fun(GX, GY) -> [{GX-2,GY-6}, {GX-2,GY-5}, {GX-2,GY-4}] end, 'Up'},  % Right side, moving up
        {6, fun(GX, _GY) -> [{GX-2,5}, {GX-2,4}, {GX-2,3}] end, 'Down'},  % Right side, moving down
        {7, fun(GX, _GY) -> [{GX-6,1}, {GX-5,1}, {GX-4,1}] end, 'Left'}  % Top-right, moving left
    ].

generate_new_snake_position(SnakeId, NumNewPlayers, {GridX, GridY} = GridSize) ->
    PositionIndex = NumNewPlayers rem ?NUM_STARTING_POSITIONS,
    PositionConfig = get_starting_position_config(),
    {PositionIndex, PositionGenerator, InitialDirection} = 
        lists:keyfind(PositionIndex, 1, PositionConfig),
    StartingCoordinates = PositionGenerator(GridX, GridY),
    {SnakeId, StartingCoordinates, InitialDirection}.
```

**Alternative: Even Cleaner with Separate Module**
```erlang
% src/snake_positions.erl
-module(snake_positions).
-export([generate_new_position/3]).
-include("game_config.hrl").

-type coordinate() :: {non_neg_integer(), non_neg_integer()}.
-type direction() :: 'Up' | 'Down' | 'Left' | 'Right'.
-type grid_size() :: {pos_integer(), pos_integer()}.

-spec generate_new_position(SnakeId :: atom(), 
                            NumPlayers :: non_neg_integer(), 
                            GridSize :: grid_size()) -> 
    {atom(), [coordinate()], direction()}.

generate_new_position(SnakeId, NumNewPlayers, GridSize) ->
    PositionIndex = NumNewPlayers rem ?NUM_STARTING_POSITIONS,
    {Coordinates, Direction} = get_position_for_index(PositionIndex, GridSize),
    {SnakeId, Coordinates, Direction}.

%% Private functions - one per starting position for clarity
get_position_for_index(0, _GridSize) -> 
    {[{3,1}, {2,1}, {1,1}], 'Right'};
get_position_for_index(1, _GridSize) -> 
    {[{1,5}, {1,4}, {1,3}], 'Down'};
get_position_for_index(2, {_GridX, GridY}) -> 
    {[{1,GridY-2}, {1,GridY-3}, {1,GridY-4}], 'Up'};
get_position_for_index(3, {_GridX, GridY}) -> 
    {[{5,GridY-2}, {4,GridY-2}, {3,GridY-2}], 'Right'};
get_position_for_index(4, {GridX, GridY}) -> 
    {[{GridX-4,GridY-2}, {GridX-3,GridY-2}, {GridX-2,GridY-2}], 'Left'};
get_position_for_index(5, {GridX, GridY}) -> 
    {[{GridX-2,GridY-6}, {GridX-2,GridY-5}, {GridX-2,GridY-4}], 'Up'};
get_position_for_index(6, {GridX, _GridY}) -> 
    {[{GridX-2,5}, {GridX-2,4}, {GridX-2,3}], 'Down'};
get_position_for_index(7, {GridX, _GridY}) -> 
    {[{GridX-6,1}, {GridX-5,1}, {GridX-4,1}], 'Left'}.
```

**Benefits:**
- Much easier to add new starting positions
- Clear comments explain each position
- Can move to separate module for better organization
- Each position function is independently testable
- Type specifications improve documentation

---

## Example 6: Adding Type Specifications

### Before
```erlang
move_snakes([#snake{id=SnakeId, direction=D} = Snake | OtherSnakes], MoveQueue, DoneSnakes) ->
    % ... implementation
```

### After
```erlang
-type snake_id() :: atom().
-type direction() :: 'Up' | 'Down' | 'Left' | 'Right' | undefined.
-type move_queue() :: [{snake_id(), queue:queue()}].
-type snake_list() :: [#snake{}].

-spec move_snakes(snake_list(), move_queue(), snake_list()) -> 
    {snake_list(), move_queue()}.
move_snakes([#snake{id=SnakeId, direction=Direction} = Snake | OtherSnakes], 
            MoveQueue, 
            ProcessedSnakes) ->
    % ... implementation with improved variable names
```

---

## Example 7: Removing Dead Code

### Files to Clean
**snake_ui.erl:**
- Remove lines 108-109 (commented `get_snake` function)
- Remove lines 169-179 (commented `display_snakes` and `snake` functions)

**game_logic.erl:**
- Remove lines 702-703 (commented `display_board` function)

**game_manager.erl:**
- Clean up line 227 (commented timeout, document why infinity is used)

### Example Change
```erlang
% Before (game_manager.erl, lines 226-230):
    after
        %% 10000 ->
        infinity ->
            fail
    end;

% After with documentation:
    after
        ?JOIN_TIMEOUT_MS ->  % Set to infinity to wait indefinitely for game join
            fail
    end;
```

---

## Implementation Strategy

### Phase 1: Quick Wins (1-2 days)
1. Create `game_config.hrl` with all constants
2. Update all files to use named constants
3. Remove all commented/dead code
4. Improve variable names in most critical functions

### Phase 2: Structural Improvements (3-5 days)
1. Extract message handlers from `game_loop/2`
2. Simplify nested conditionals in `snake_ui.erl`
3. Refactor position generation logic
4. Split large functions into smaller ones

### Phase 3: Documentation & Types (2-3 days)
1. Add type specifications to all exported functions
2. Add module-level documentation
3. Document complex algorithms
4. Add function-level comments where logic is non-obvious

### Phase 4: Testing & Validation (2-3 days)
1. Ensure all refactored code works identically to original
2. Run existing tests (if any)
3. Manual testing of game functionality
4. Performance validation

## Conclusion

These examples demonstrate how to:
- Break down complex functions into smaller, focused ones
- Use meaningful names for better code comprehension
- Eliminate magic numbers with named constants
- Reduce nesting depth for better readability
- Add type specifications for better documentation
- Remove dead code for clarity

Each refactoring maintains the same functionality while significantly improving code quality and maintainability.
