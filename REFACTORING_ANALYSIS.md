# Snake Game Code Refactoring Analysis

## Executive Summary
The codebase contains several instances of "snaked code" - overly complex, deeply nested, and hard-to-follow code that needs refactoring for improved maintainability and clarity.

## Critical Issues Identified

### 1. **Massive Functions with Deep Nesting**

#### `game_loop/2` in `game_logic.erl` (Lines 176-373)
- **Problem**: 197 lines with deeply nested `receive` blocks and complex pattern matching
- **Impact**: Extremely difficult to understand, test, and maintain
- **Severity**: CRITICAL

**Specific Issues:**
```erlang
% Lines 199-362: Single receive block with 15+ different message patterns
% Each pattern has multiple levels of nested case statements
```

**Recommendation:**
- Extract message handlers into separate functions
- Create dedicated handler functions: `handle_tick/2`, `handle_move/3`, `handle_kill_snake/2`, etc.
- Use a message dispatcher pattern

**Example Refactoring:**
```erlang
game_loop(GameState, ReceivedMoveQueue) ->
    receive
        {tick, NewClock, Options} = Tick ->
            handle_tick_message(Tick, GameState, ReceivedMoveQueue);
        {move, SnakeId, Clock, MoveList} ->
            handle_move_message(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue);
        {kill_snake, SnakeId} ->
            handle_kill_snake(SnakeId, GameState, ReceivedMoveQueue);
        % ... other message patterns
    end.

handle_tick_message({tick, NewClock, Options}, GameState, ReceivedMoveQueue) ->
    #game_state{clock=Clock, myid=MyId, snakes=Snakes} = GameState,
    case get_missing_snakes(Snakes) of
        [] -> handle_tick_with_all_events(NewClock, Options, GameState, ReceivedMoveQueue);
        MissingSnakes -> handle_tick_with_missing_events(MissingSnakes, NewClock, GameState, ReceivedMoveQueue)
    end.
```

### 2. **Magic Numbers Without Named Constants**

**Found in multiple files:**
- `50` - timeout/delay value (lines 347, 360 in game_logic.erl)
- `8` - max players (hardcoded in multiple places, defined only in game_manager.erl)
- `5000` - default timeout (game_manager.erl:9)
- `100` - food score value (game_logic.erl:601)
- `10` - grid multiplier (snake_ui.erl:18, 113)
- `750`, `550` - window dimensions (snake_ui.erl:17)

**Recommendation:**
Create a configuration header file:
```erlang
% game_config.hrl
-define(MAX_PLAYERS, 8).
-define(TICK_RETRY_DELAY_MS, 50).
-define(TICK_RETRY_LIMIT, 50).
-define(DEFAULT_MANAGER_TIMEOUT_MS, 5000).
-define(FOOD_SCORE_VALUE, 100).
-define(GRID_CELL_SIZE, 10).
-define(WINDOW_WIDTH, 750).
-define(WINDOW_HEIGHT, 550).
-define(TCP_SEND_TIMEOUT_MS, 3000).
```

### 3. **Poor Variable Naming**

**Problematic variables:**
- `GS` → `GameState` (game_logic.erl:502-506)
- `D` → `Direction` (game_logic.erl:659, 669, 688)
- `P` → `Position` or `Priority` (context-dependent)
- `L` → `Length` or `Lives` (context-dependent)
- `Q`, `Q1`, `Q2` → descriptive queue names
- `RMQ` → `ReceivedMoveQueue` (already used elsewhere, be consistent)

**Example:**
```erlang
% Before
move_snake(#snake{position=Q, length=L} = Snake, D) ->
    Fun = move_snake_function(D),
    Q1 = add_to_front(Fun(front(Q)), Q),
    Snake#snake{position=resize_snake_position(Q1, L), direction=D}.

% After
move_snake(#snake{position=PositionQueue, length=Length} = Snake, Direction) ->
    MovementFunction = move_snake_function(Direction),
    NewHeadPosition = MovementFunction(front(PositionQueue)),
    UpdatedQueue = add_to_front(NewHeadPosition, PositionQueue),
    ResizedQueue = resize_snake_position(UpdatedQueue, Length),
    Snake#snake{position=ResizedQueue, direction=Direction}.
```

### 4. **Complex Nested Conditionals**

#### `snak/2` in `snake_ui.erl` (Lines 143-166)
- **Problem**: Deeply nested case statements within case statements
- **Impact**: Difficult to trace logic flow

**Recommendation:**
Extract color assignment logic:
```erlang
snak(#snake{position=P, id=Id}, Can) ->
    Coords = queue:to_list(P),
    Length = length(Coords),
    Color = get_or_assign_snake_color(Id),
    create_snake_graphic(Coords, Length, Color, Can).

get_or_assign_snake_color(SnakeId) ->
    ColorsList = get(list_Of_Colors),
    case lists:keysearch(SnakeId, 2, ColorsList) of
        {value, {Color, _}} -> 
            Color;
        false -> 
            assign_new_color_to_snake(SnakeId, ColorsList)
    end.

assign_new_color_to_snake(SnakeId, ColorsList) ->
    {value, {Color, none}} = lists:keysearch(none, 2, ColorsList),
    NewColorsList = lists:keyreplace(Color, 1, ColorsList, {Color, SnakeId}),
    put(list_Of_Colors, NewColorsList),
    Color.

create_snake_graphic([], _Length, _Color, _Can) -> 
    false;
create_snake_graphic([{X,Y}], 1, Color, Can) ->
    Coords = resize([{X,Y}, {X,Y}]),
    gs:create(line, Can, [{coords, Coords}, {fg, Color}, {width, 10}]);
create_snake_graphic(Coords, _Length, Color, Can) ->
    gs:create(line, Can, [{coords, resize(Coords)}, {fg, Color}, {width, 10}]).
```

### 5. **Dead Code and Commented Sections**

**Should be removed:**
- Lines 108-109, 169-179 in `snake_ui.erl` - commented functions
- Line 702-703 in `game_logic.erl` - commented display_board function
- Lines 227-228 in `game_manager.erl` - commented timeout

**Recommendation:** 
Remove all commented code. Use version control (git) for history.

### 6. **Overly Complex Pattern Matching**

#### `generate_new_snake_position/3` (Lines 388-407)
- **Problem**: Long case statement with 8 branches for positioning
- **Impact**: Hard to verify correctness, modify, or extend

**Recommendation:**
Use a lookup table or data structure:
```erlang
-define(SNAKE_START_POSITIONS, [
    {0, fun({GridX, _GridY}) -> {[{3,1},{2,1},{1,1}], 'Right'} end},
    {1, fun({_GridX, _GridY}) -> {[{1,5},{1,4},{1,3}], 'Down'} end},
    {2, fun({_GridX, GridY}) -> {[{1,GridY-2},{1,GridY-3},{1,GridY-4}], 'Up'} end},
    {3, fun({_GridX, GridY}) -> {[{5,GridY-2},{4,GridY-2},{3,GridY-2}], 'Right'} end},
    {4, fun({GridX, GridY}) -> {[{GridX-4,GridY-2},{GridX-3,GridY-2},{GridX-2,GridY-2}], 'Left'} end},
    {5, fun({GridX, GridY}) -> {[{GridX-2,GridY-6},{GridX-2,GridY-5},{GridX-2,GridY-4}], 'Up'} end},
    {6, fun({GridX, _GridY}) -> {[{GridX-2,5},{GridX-2,4},{GridX-2,3}], 'Down'} end},
    {7, fun({GridX, _GridY}) -> {[{GridX-6,1},{GridX-5,1},{GridX-4,1}], 'Left'} end}
]).

generate_new_snake_position(SnakeId, NumNewPlayers, GridSize) ->
    PositionIndex = NumNewPlayers rem 8,
    {PositionIndex, PositionFun} = lists:keyfind(PositionIndex, 1, ?SNAKE_START_POSITIONS),
    {Position, Direction} = PositionFun(GridSize),
    {SnakeId, Position, Direction}.
```

### 7. **Functions Doing Too Much**

#### `evaluate_food/4` (Lines 632-643)
Combines iteration, snake feeding, and result collection

**Recommendation:**
Separate concerns:
```erlang
% Separate the iteration from the feeding logic
evaluate_food(Snakes, Foods) ->
    {UpdatedSnakes, RemainingFoods, FeedingResults} = 
        lists:foldl(fun evaluate_single_snake_feeding/2, 
                    {[], Foods, []}, 
                    Snakes),
    {UpdatedSnakes, RemainingFoods, FeedingResults}.

evaluate_single_snake_feeding(Snake, {AccSnakes, AvailableFoods, AccResults}) ->
    {FeedingResult, {UpdatedSnake, RemainingFoods}} = feed_snake(Snake, AvailableFoods),
    NewResults = case FeedingResult of
        fed -> [{fed, Snake#snake.id} | AccResults];
        not_fed -> AccResults
    end,
    {[UpdatedSnake | AccSnakes], RemainingFoods, NewResults}.
```

### 8. **Inconsistent Error Handling**

**Issues:**
- Some functions use `{ok, Result}` / `{error, Reason}` tuples
- Others use pattern matching with `false` / actual value
- Some crash on errors, others return error tuples

**Recommendation:**
Establish consistent error handling patterns across the codebase.

### 9. **Process Dictionary Overuse**

**Heavy use of `put/get` throughout:**
- Lines 73-78 in game_logic.erl
- Lines 34, 155-156, 198-200 in snake_ui.erl
- Lines 38-39, 163-172, 279, 306, 318-346 in game_manager.erl

**Problem:**
- Makes code harder to test
- Hidden state dependencies
- Non-obvious side effects

**Recommendation:**
Where possible, pass state explicitly through function parameters or use proper state management (gen_server behavior).

### 10. **Lack of Type Specifications**

**Problem:**
No `-spec` declarations for any functions

**Recommendation:**
Add type specifications for better documentation and dialyzer support:
```erlang
-spec move_snake(Snake :: #snake{}, Direction :: direction()) -> #snake{}.
-spec generate_new_snake_position(SnakeId :: atom(), NumPlayers :: non_neg_integer(), 
                                   GridSize :: {pos_integer(), pos_integer()}) -> 
                                   {atom(), [coordinate()], direction()}.
```

## Priority Recommendations

### High Priority (Do First)
1. **Extract message handlers from `game_loop/2`** - Makes the most critical code readable
2. **Define named constants** - Quick win, improves readability throughout
3. **Improve variable naming** - Another quick win for readability
4. **Remove dead code** - Clean up clutter

### Medium Priority
5. **Simplify nested conditionals** - Improves maintainability
6. **Refactor complex functions** - Break down into smaller pieces
7. **Add type specifications** - Better documentation

### Low Priority (Nice to Have)
8. **Reduce process dictionary usage** - Requires larger architectural changes
9. **Standardize error handling** - Consistency improvement
10. **Extract lookup tables** - Code organization

## Metrics Summary

| Metric | Current | Target | Notes |
|--------|---------|--------|-------|
| Longest function | 197 lines | < 50 lines | `game_loop/2` |
| Max nesting depth | 6-7 levels | < 4 levels | Various receive/case blocks |
| Magic numbers | 15+ | 0 | Replace with named constants |
| Single-letter vars | 20+ | 0 | Use descriptive names |
| Commented code | 10+ sections | 0 | Remove dead code |

## Conclusion

The codebase would benefit significantly from refactoring focused on:
1. **Breaking down large functions** into smaller, focused pieces
2. **Eliminating magic numbers** with named constants
3. **Improving naming** for better code comprehension
4. **Reducing nesting** through extraction and early returns
5. **Removing dead code** for clarity

These changes will make the code easier to understand, test, maintain, and extend without changing its functionality.
