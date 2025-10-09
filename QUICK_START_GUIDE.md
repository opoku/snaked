# Quick Start Guide: Improving the Snake Code

## TL;DR - What's Wrong?

The codebase has "snaked code" - overly complex, hard-to-follow code that needs refactoring. Think of a snake that's all tangled up on itself!

## The Big Three Issues

### 1. 🐍 Giant Snake Function (`game_loop/2`)
- **197 lines** of tangled logic
- **15+ different message types** in one massive receive block
- **Impossible to understand** without serious mental gymnastics

### 2. 🔢 Magic Numbers Everywhere
```erlang
50   -> What is this? Timeout? Delay? Retries?
100  -> Score? Points? Size?
8    -> Players? Width? Height?
```

### 3. 🔤 Cryptic Variable Names
```erlang
D    -> Direction? Distance? Data?
Q    -> Queue? Query? Quotient?
GS   -> GameState? Graphics? GameServer?
L    -> Length? Lives? List? Level?
```

## Start Here: 5-Minute Improvement

The absolute fastest way to improve the code:

### Step 1: Create Constants File (15 minutes)
Create `src/game_config.hrl`:
```erlang
-define(MAX_PLAYERS, 8).
-define(TICK_RETRY_DELAY_MS, 50).
-define(FOOD_SCORE_VALUE, 100).
-define(GRID_CELL_SIZE_PX, 10).
```

### Step 2: Use Constants (10 minutes per file)
Replace in each .erl file:
```erlang
% Before:
PlayerCount < 8

% After:
-include("game_config.hrl").
PlayerCount < ?MAX_PLAYERS
```

### Step 3: Rename One Function's Variables (20 minutes)
Pick the worst function and rename:
```erlang
% Before:
move_snake(#snake{position=Q, length=L} = Snake, D) ->
    Fun = move_snake_function(D),
    Q1 = add_to_front(Fun(front(Q)), Q),
    Snake#snake{position=resize_snake_position(Q1, L), direction=D}.

% After:
move_snake(#snake{position=PositionQueue, length=SnakeLength} = Snake, Direction) ->
    MovementFunction = move_snake_function(Direction),
    NewHeadPosition = MovementFunction(front(PositionQueue)),
    ExtendedQueue = add_to_front(NewHeadPosition, PositionQueue),
    TrimmedQueue = resize_snake_position(ExtendedQueue, SnakeLength),
    Snake#snake{position=TrimmedQueue, direction=Direction}.
```

**Result:** You've already made the code noticeably better in under an hour!

## The 1-Day Improvement Plan

If you have a full day to dedicate:

### Morning (4 hours)
1. **Create `game_config.hrl`** (30 min)
2. **Replace all magic numbers** (2 hours)
3. **Remove all commented code** (30 min)
4. **Test that game still works** (1 hour)

### Afternoon (4 hours)
5. **Rename variables in `game_logic.erl`** (2 hours)
6. **Rename variables in `snake_ui.erl`** (1 hour)
7. **Test thoroughly** (1 hour)

**Result:** The code will be dramatically more readable!

## The 1-Week Improvement Plan

### Monday: Foundation
- Create constants file
- Replace all magic numbers
- Remove dead code

### Tuesday-Wednesday: Variable Names
- Rename all single-letter variables
- Use descriptive names throughout
- Update comments

### Thursday-Friday: Function Extraction
- Extract handlers from `game_loop/2`
- Break down other long functions
- Simplify nested conditionals

**Result:** Professional-quality, maintainable code!

## Top 5 Functions to Refactor

In order of impact:

### 1. `game_loop/2` (Lines 176-373) - game_logic.erl
**Why:** It's the core game logic and it's a nightmare
**Impact:** ⭐⭐⭐⭐⭐
**Difficulty:** Hard
**Time:** 4-6 hours

### 2. `move_snake/4` (Lines 659-674) - game_logic.erl  
**Why:** Central to game mechanics, bad variable names
**Impact:** ⭐⭐⭐⭐
**Difficulty:** Easy
**Time:** 20 minutes

### 3. `snak/2` (Lines 143-166) - snake_ui.erl
**Why:** Deeply nested, handles both color and rendering
**Impact:** ⭐⭐⭐
**Difficulty:** Medium
**Time:** 2-3 hours

### 4. `generate_new_snake_position/3` (Lines 388-407) - game_logic.erl
**Why:** Long case statement, hard to verify correctness
**Impact:** ⭐⭐⭐
**Difficulty:** Medium  
**Time:** 2 hours

### 5. `game_manager_loop/1` (Lines 263-420) - game_manager.erl
**Why:** Another massive receive block
**Impact:** ⭐⭐⭐
**Difficulty:** Hard
**Time:** 4-6 hours

## Magic Numbers to Replace

Quick reference of what each number means:

| Number | Meaning | Suggested Constant |
|--------|---------|-------------------|
| 8 | Max players | `?MAX_PLAYERS` |
| 50 | Tick retry delay (ms) | `?TICK_RETRY_DELAY_MS` |
| 50 | Max tick retries | `?MAX_TICK_RETRIES` |
| 100 | Food score value | `?FOOD_SCORE_VALUE` |
| 10 | Grid cell size (pixels) | `?GRID_CELL_SIZE_PX` |
| 750 | Window width (pixels) | `?WINDOW_WIDTH_PX` |
| 550 | Window height (pixels) | `?WINDOW_HEIGHT_PX` |
| 5000 | Manager timeout (ms) | `?DEFAULT_MANAGER_TIMEOUT_MS` |
| 3000 | TCP send timeout (ms) | `?TCP_SEND_TIMEOUT_MS` |

## Variable Naming Cheat Sheet

| Bad | Good | When to Use |
|-----|------|-------------|
| `D` | `Direction` | Movement direction |
| `Q` | `PositionQueue` | Queue of positions |
| `Q1` | `UpdatedQueue` or `NewQueue` | Modified queue |
| `L` | `Length` or `Lives` | Specify which! |
| `P` | `Position` or `Priority` | Specify which! |
| `GS` | `GameState` | Game state record |
| `RMQ` | Use fully: `ReceivedMoveQueue` | Consistency |
| `Can` | `Canvas` | Graphics canvas |
| `Coords` | `Coordinates` | Spell it out |
| `Len` | `Length` | Spell it out |

## Dead Code to Remove

Just delete these lines (git remembers!):

**snake_ui.erl:**
- Lines 108-109: Commented `get_snake/2`
- Lines 169-179: Commented alternative implementations

**game_logic.erl:**
- Lines 702-703: Commented `display_board/2`

**game_manager.erl:**
- Line 227: Commented timeout (document why infinity is used instead)

## Testing After Each Change

### Minimal Test (5 minutes):
```bash
cd src
make clean
make
cd ..
./run.sh
# Play the game for a minute
# Try all four directions
# Eat some food
# Check score updates
```

### Thorough Test (15 minutes):
- Start a game
- Join with another player (if possible)
- Test all four directions
- Collect food
- Die and respawn
- Check score display
- Verify colors work

## Before/After Example

### Before: Impossible to Understand
```erlang
game_loop(#game_state{state=started} = GS, RMQ) ->
    #game_state{clock=C, myid = MyId} = GS,
    NC = C + 1,
    receive
        {move, SId, C, ML} -> 
            ?LOG("move, movelist--> ~p~n", [ML]),
            put(expected_events, get(expected_events) -- [SId]),
            Snakes = GS#game_state.snakes,
            case {game_manager:is_leader(), lists:keyfind(SId, #snake.id, Snakes)} of
                {true, #snake{length=0}} ->
                    NP = GS#game_state.new_player_positions,
                    GrS = GS#game_state.size,
                    NP1 = [generate_new_snake_position(SId, length(NP), GrS) | NP],
                    game_loop(GS#game_state{new_player_positions=NP1}, RMQ);
                % ... 30 more lines
```

### After: Clear and Readable
```erlang
game_loop(#game_state{state=started} = GameState, ReceivedMoveQueue) ->
    receive
        {move, SnakeId, Clock, MoveList} -> 
            handle_move_event(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue);
        % ... other message handlers
    end.

handle_move_event(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue) ->
    ?LOG("Snake ~p move received: ~p~n", [SnakeId, MoveList]),
    remove_from_expected_events(SnakeId),
    
    Snake = find_snake(SnakeId, GameState#game_state.snakes),
    IsLeader = game_manager:is_leader(),
    
    case classify_move_type(Snake, IsLeader) of
        new_player_as_leader ->
            handle_new_player_move_as_leader(SnakeId, GameState, ReceivedMoveQueue);
        new_player_as_follower ->
            handle_new_player_move_as_follower(GameState, ReceivedMoveQueue);
        existing_player ->
            handle_existing_player_move(SnakeId, Clock, MoveList, GameState, ReceivedMoveQueue);
        unregistered ->
            handle_unregistered_snake_error(SnakeId, GameState, ReceivedMoveQueue)
    end.
```

## Common Questions

**Q: Will this change how the game works?**
A: No! These are refactorings - same behavior, clearer code.

**Q: What if I break something?**
A: That's what testing is for. Make small changes, test frequently, use git.

**Q: Where do I start?**
A: Start with the "5-Minute Improvement" above. Quick win!

**Q: How long will this take?**
A: Depends on how thorough you want to be:
- 1 hour: Noticeable improvement
- 1 day: Major improvement
- 1 week: Professional quality

**Q: Do I have to do everything?**
A: No! Even just adding constants and renaming variables helps tremendously.

## Success Criteria

You'll know you've succeeded when:
- [ ] New team members can understand the code
- [ ] You can find and fix bugs faster
- [ ] Adding features is straightforward
- [ ] No one asks "what does this variable mean?"
- [ ] Functions fit on one screen
- [ ] Code reviews are pleasant, not painful

## Get Started Now!

1. Read `IMPROVEMENT_SUMMARY.md` for the full plan
2. Check `REFACTORING_EXAMPLES.md` for detailed code examples
3. Review `REFACTORING_ANALYSIS.md` for in-depth analysis
4. Start with the "5-Minute Improvement" above
5. Work incrementally
6. Test frequently
7. Celebrate improvements!

Remember: **Perfect is the enemy of good.** Any improvement is better than none!
