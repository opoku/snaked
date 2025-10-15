# Comprehensive Bug Report - Distributed Snake Game

**Date:** 2025-10-15  
**Project:** Distributed Snake Game (Erlang Implementation)  
**Status:** "It also barely works" (per README)

---

## Executive Summary

This distributed snake game has **multiple critical bugs** that affect game state consistency, player synchronization, resource management, and overall stability. The codebase contains race conditions, improper error handling, potential memory leaks, and logic errors that can cause crashes or undefined behavior.

**Severity Levels:**
- 🔴 **CRITICAL**: Can cause crashes, data corruption, or complete system failure
- 🟠 **HIGH**: Significantly impacts functionality or user experience
- 🟡 **MEDIUM**: Noticeable issues but workarounds may exist
- 🟢 **LOW**: Minor issues with minimal impact

---

## CRITICAL BUGS (🔴)

### BUG-001: Race Condition in Player Addition (game_manager.erl)
**Severity:** 🔴 CRITICAL  
**Location:** `game_manager.erl:191-212, 298-347`  
**Category:** Concurrency/Race Condition

**Description:**  
The `try_to_add_new_player/1` function spawns a background process that acquires a lock, but there's a race condition between:
1. Checking player count (`PlayerCount < ?MAX_PLAYERS`)
2. Actually adding the player to game state
3. Multiple nodes receiving player_added acknowledgments

The ACK tracking mechanism uses process dictionary (`put({NodeId, addplayer}, IdList)`) which can be corrupted if multiple add_player operations occur simultaneously.

**Impact:**
- Multiple players could be added beyond MAX_PLAYERS limit
- Game state corruption (as mentioned in bugs.txt: "somehow the game state got corrupted")
- Orphaned entries in process dictionary leading to memory leaks

**Evidence from bugs.txt:**
```
-there is an issue where osei and manan were playing the game thinking they are
the only players in the game.. somehow the game state got corrupted
```

**Suggested Fix:**
1. Use gen_server for game_manager to ensure sequential message processing
2. Move ACK tracking from process dictionary to manager state record
3. Add validation after lock acquisition to re-check player count
4. Implement proper rollback mechanism if player addition fails

---

### BUG-002: Infinite Reconnection Loop on TCP Connect Failure
**Severity:** 🔴 CRITICAL  
**Location:** `tcp_comm.erl:18-26`  
**Category:** Network/Infinite Loop

**Description:**  
The `client_connect/2` function has infinite recursion with no timeout or backoff:

```erlang
client_connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, [...]) of
        {ok, Socket}-> Socket;
        {error, Reason}->?LOG("Error on connect Socket ~p. Trying again~n", [Reason]), 
                         client_connect(Host, Port)  % INFINITE RECURSION
    end.
```

**Impact:**
- System can hang indefinitely trying to connect to unreachable host
- CPU consumption spikes to 100%
- Process mailbox can overflow if error messages accumulate
- No way to gracefully fail or timeout

**Suggested Fix:**
```erlang
client_connect(Host, Port, Retries) when Retries > 0 ->
    case gen_tcp:connect(Host, Port, [...]) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} ->
            ?LOG("Error on connect Socket ~p. ~p retries left~n", [Reason, Retries-1]),
            timer:sleep(min(1000 * (10 - Retries), 5000)), % Exponential backoff
            client_connect(Host, Port, Retries - 1)
    end;
client_connect(_Host, _Port, 0) ->
    {error, connection_failed}.
```

---

### BUG-003: Process Dictionary Memory Leak
**Severity:** 🔴 CRITICAL  
**Location:** Multiple files: `game_manager.erl:279, 306, 332, 337`  
**Category:** Memory Leak

**Description:**  
The code uses process dictionary extensively but doesn't clean up entries:
- `put(NodeId, Pid)` at line 279 - never erased if connection fails
- `put({NodeId, addplayer}, IdList)` at line 306 - only erased on success path
- Temporary node tracking in game_logic never cleaned up properly

**Impact:**
- Memory leak grows with each failed connection or player addition
- Eventually causes out-of-memory errors
- Performance degradation over time

**Suggested Fix:**
1. Use record-based state instead of process dictionary
2. Add cleanup code in error paths:
```erlang
try
    try_to_add_new_player(NodeId)
after
    erase(NodeId),
    erase({NodeId, addplayer}),
    message_passer:release_lock(add_player)
end
```

---

### BUG-004: Unsafe Pattern Matching in game_logic:move_snakes
**Severity:** 🔴 CRITICAL  
**Location:** `game_logic.erl:659-671`  
**Category:** Runtime Crash

**Description:**  
The code assumes `lists:keyfind/3` will always succeed:

```erlang
move_snakes([#snake{id=SnakeId, direction=D} = Snake | OtherSnakes], MoveQueue, DoneSnakes) ->
    {SnakeId, Queue} = lists:keyfind(SnakeId, 1, MoveQueue),  % CAN RETURN 'false'!
```

If the MoveQueue is out of sync with the snake list, `lists:keyfind` returns `false`, causing a badmatch crash.

**Impact:**
- Game crashes with badmatch error
- All connected players disconnected
- Loss of game state

**Suggested Fix:**
```erlang
move_snakes([#snake{id=SnakeId, direction=D} = Snake | OtherSnakes], MoveQueue, DoneSnakes) ->
    case lists:keyfind(SnakeId, 1, MoveQueue) of
        {SnakeId, Queue} ->
            % existing logic
            ...;
        false ->
            ?LOG("ERROR: No move queue for snake ~p~n", [SnakeId]),
            move_snakes(OtherSnakes, MoveQueue, [Snake | DoneSnakes])
    end;
```

---

### BUG-005: Message Reordering Bug in Reliable Multicast
**Severity:** 🔴 CRITICAL  
**Location:** `message_passer.erl:332-422`  
**Category:** Distributed Systems/Causality Violation

**Description:**  
The reliable multicast implementation has a subtle bug in the ACK processing logic. At line 390, it searches for a message in the ACK list:

```erlang
case find_source_message({Source,MsgId}, NewAckList) of
    found -> loop(...);
    not_found ->
        % Process message from HoldQueue
```

However, this can process messages out of order if ACKs arrive before the corresponding multicast message (lines 378-383 have a workaround with `erlang:send_after`, but it's insufficient).

**Impact:**
- Game state inconsistency across nodes
- Lock protocol violations
- Food generation desynchronization

**Suggested Fix:**
Implement a proper vector clock or lamport timestamp ordering with a guaranteed delivery queue.

---

## HIGH SEVERITY BUGS (🟠)

### BUG-006: Incorrect Snake Collision Detection
**Severity:** 🟠 HIGH  
**Location:** `game_logic.erl:522-533`  
**Category:** Logic Error

**Description:**  
The collision detection has a critical flaw:

```erlang
detect_collision(Snake, ObstacleMap) ->
    #snake{id=SnakeId, position=SnakePos} = Snake,
    case dict:find(front(SnakePos), ObstacleMap) of
        {ok, [SnakeId]} ->
            false;  % Snake's own head - not a collision
        _Any ->
            true    % Everything else is a collision
    end.
```

**Problem:** This returns `true` (collision) even when the dict:find returns `error` (key not found), meaning the snake is in empty space!

**Impact:**
- Snakes die randomly in empty space
- Game is unplayable in many areas of the grid
- Inconsistent behavior across nodes

**Suggested Fix:**
```erlang
detect_collision(Snake, ObstacleMap) ->
    #snake{id=SnakeId, position=SnakePos} = Snake,
    case dict:find(front(SnakePos), ObstacleMap) of
        {ok, [SnakeId]} ->
            false;  % Snake's own head
        {ok, _} ->
            true;   % Collision with obstacle or other snake
        error ->
            false   % Empty space - no collision
    end.
```

---

### BUG-007: Food Generation Infinite Loop
**Severity:** 🟠 HIGH  
**Location:** `food.erl:9-22`  
**Category:** Infinite Loop/Stack Overflow

**Description:**  
The food generation uses unbounded recursion when grid is full:

```erlang
generate_foods(GameState) ->
    % ... generate random position
    case is_block_occupied(GameState, X, Y) of
        true ->
            generate_foods(GameState);  % TAIL RECURSION WITH NO LIMIT
        false ->
            % create food
    end.
```

**Impact:**
- If grid is mostly full, this can recurse thousands of times
- Stack overflow when grid is nearly full
- Game freezes during food generation
- Leader node becomes unresponsive

**Suggested Fix:**
```erlang
generate_foods(GameState) ->
    generate_foods(GameState, 100).  % Max 100 attempts

generate_foods(_GameState, 0) ->
    ?LOG("Failed to generate food after max attempts~n", []),
    GameState;
generate_foods(GameState, AttemptsLeft) ->
    #game_state{size = {XSize, YSize}} = GameState,
    Offset = generate_random_number(XSize*YSize - 1),
    X = Offset rem XSize,
    Y = Offset div XSize,
    case is_block_occupied(GameState, X, Y) of
        true ->
            generate_foods(GameState, AttemptsLeft - 1);
        false ->
            % create food
    end.
```

---

### BUG-008: Random Seed Not Per-Process
**Severity:** 🟠 HIGH  
**Location:** `food.erl:29-32`  
**Category:** Randomness/Predictability

**Description:**  
The random number generator is seeded with `now()` on every call:

```erlang
generate_random_number(N) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),  % Reseeds EVERY time
    random:uniform(N).
```

**Problems:**
1. `now()` has been deprecated since Erlang 18.0 (replaced by erlang:timestamp())
2. Reseeding on every call defeats the purpose of pseudo-random generation
3. If called multiple times in quick succession, seeds may be identical
4. All nodes may generate same food positions at same clock tick

**Impact:**
- Predictable food positions
- Multiple foods spawned at same location
- Poor randomness quality

**Suggested Fix:**
```erlang
% In init:
ensure_random_seeded() ->
    case get(random_seeded) of
        true -> ok;
        _ -> 
            rand:seed(exsplus),  % Use newer rand module
            put(random_seeded, true)
    end.

generate_random_number(N) ->
    ensure_random_seeded(),
    rand:uniform(N).
```

---

### BUG-009: Missing Events Detection Incomplete
**Severity:** 🟠 HIGH  
**Location:** `game_logic.erl:377-386`  
**Category:** Logic Error

**Description:**  
The `get_missing_snakes/1` function has flawed logic:

```erlang
get_missing_snakes(SnakeList) ->
    case get(expected_events) of
        [] -> [];
        ExpectedEvents ->
            SnakeList1 = lists:filter(fun (#snake{id=SnakeId,position=Pos}) ->
                lists:member(SnakeId, ExpectedEvents) and not(queue:is_empty(Pos))
            end, SnakeList),
            [SnakeId || #snake{id=SnakeId} <- SnakeList1 ]
    end.
```

**Problem:** The `not(queue:is_empty(Pos))` condition means new snakes with empty positions are never considered missing, even if they haven't sent events.

**Impact:**
- New players joining game can cause indefinite waits
- Clock pauses unnecessarily
- Game freezes for all players

**Suggested Fix:**
```erlang
get_missing_snakes(SnakeList) ->
    case get(expected_events) of
        [] -> [];
        ExpectedEvents ->
            [SnakeId || #snake{id=SnakeId, position=Pos} <- SnakeList,
                        lists:member(SnakeId, ExpectedEvents),
                        not queue:is_empty(Pos) orelse is_new_player(SnakeId)]
    end.
```

---

### BUG-010: Lock State Not Cleaned on Node Failure
**Severity:** 🟠 HIGH  
**Location:** `message_passer.erl:616-622`  
**Category:** Distributed Lock/Deadlock

**Description:**  
The `delete_node_from_lockedstatelist/2` function removes a dead node from the lock state, but doesn't check if the lock should be granted to waiting processes:

```erlang
delete_node_from_lockedstatelist(Elem,NodeId) ->
    {ResourceId, LockState, RequestQueue, ReplyList, ReqPid} = Elem,
    NewReplyList = lists:keydelete(NodeId, 2, ReplyList),
    % ... remove from request queue ...
    {ResourceId, LockState, NewRequestQueue, NewReplyList, ReqPid}.
```

**Problem:** If `NewReplyList` becomes empty after removing the dead node, the waiting process should receive the lock, but it doesn't.

**Impact:**
- Permanent deadlock if a node holding lock dies
- Processes wait forever for locks
- Game cannot progress

**Suggested Fix:**
```erlang
delete_node_from_lockedstatelist(Elem, NodeId) ->
    {ResourceId, LockState, RequestQueue, ReplyList, ReqPid} = Elem,
    NewReplyList = lists:keydelete(NodeId, 2, ReplyList),
    RequestList = queue:to_list(RequestQueue),
    NewRequestList = lists:filter(fun({_,HostId,_,_}) -> HostId =/= NodeId end, RequestList),
    NewRequestQueue = queue:from_list(NewRequestList),
    
    % If all replies received, grant lock
    case {LockState, NewReplyList} of
        {{wanted, _}, []} ->
            ReqPid ! {acquired, message_passer, ResourceId},
            {ResourceId, held, queue:new(), [], ReqPid};
        _ ->
            {ResourceId, LockState, NewRequestQueue, NewReplyList, ReqPid}
    end.
```

---

## MEDIUM SEVERITY BUGS (🟡)

### BUG-011: Potential Division by Zero
**Severity:** 🟡 MEDIUM  
**Location:** `food.erl:14`  
**Category:** Arithmetic Error

**Description:**  
```erlang
Offset = generate_random_number(XSize*YSize - 1),
Y = Offset div XSize,
```

If `XSize` is 0, this causes division by zero crash.

**Impact:**
- Crash when initializing game with invalid grid size
- Should be caught at configuration validation

**Suggested Fix:**
Add validation in game state initialization to ensure size > {0,0}.

---

### BUG-012: Inconsistent Error Returns
**Severity:** 🟡 MEDIUM  
**Location:** `message_passer.erl:80-87`  
**Category:** API Inconsistency

**Description:**  
The `connect/2` function returns different types:
- `NodeId` (atom) on success
- `{error}` (tuple) on failure

This inconsistency makes pattern matching difficult.

**Suggested Fix:**
```erlang
connect(Host, Port) ->
    message_passer ! {connect, self(), Host, Port},
    receive
       {message_passer, {ok, NodeId, HostInfo}} ->
            {ok, NodeId};
       {message_passer, error} ->
            {error, connection_failed}
    end.
```

---

### BUG-013: TODO Comment Indicates Known Race Condition
**Severity:** 🟡 MEDIUM  
**Location:** `game_manager.erl:11`  
**Category:** Unresolved Issue

**Description:**  
```erlang
%% TODO: race condition where the game_server's list of nodes is out of date
```

**Impact:**
- Game server may have stale node information
- Players may try to join games that no longer exist
- Synchronization issues between game_server and individual games

**Suggested Fix:**
Implement heartbeat mechanism or version numbers for game state.

---

### BUG-014: Missing Timeout in join_loop
**Severity:** 🟡 MEDIUM  
**Location:** `game_manager.erl:220-230`  
**Category:** Infinite Wait

**Description:**  
The `join_loop` has an `infinity` timeout that was commented out:

```erlang
join_loop(GameInfo, start) ->
    receive
        {hi, NodeId} ->
            ...
    after
        %% 10000 ->  % COMMENTED OUT!
        infinity ->
            fail
    end;
```

**Impact:**
- Process hangs forever if 'hi' message never arrives
- No way to recover from failed join attempt

**Suggested Fix:**
Restore the 10-second timeout or make it configurable.

---

### BUG-015: Unused Keyboard Module
**Severity:** 🟡 MEDIUM  
**Location:** `keyboard.erl:1`  
**Category:** Dead Code

**Description:**  
The keyboard module is empty and serves no purpose.

**Impact:**
- Confusion in codebase
- May indicate incomplete feature

**Suggested Fix:**
Remove the file or implement intended functionality.

---

## LOW SEVERITY BUGS (🟢)

### BUG-016: Deprecated now() Function
**Severity:** 🟢 LOW  
**Location:** `food.erl:30`, `util.erl:9,15`  
**Category:** Deprecated API

**Description:**  
Using deprecated `now()` instead of `erlang:timestamp()` or `erlang:monotonic_time()`.

**Impact:**
- Warnings in newer Erlang versions
- May be removed in future Erlang releases

**Suggested Fix:**
Replace with `erlang:timestamp()` for timestamps or `rand:seed()` for randomness.

---

### BUG-017: Hardcoded Magic Numbers
**Severity:** 🟢 LOW  
**Location:** Multiple locations  
**Category:** Code Quality

**Examples:**
- `food.erl:26` - hardcoded food lifetime of 50
- `game_logic.erl:347` - hardcoded retry count of 50
- `clock.erl:14` - CLOCK_TIME = 200
- `game_manager.erl:228` - timeout commented out

**Impact:**
- Difficult to tune game parameters
- No way to configure without code changes

**Suggested Fix:**
Move to configuration file or define as configurable parameters.

---

### BUG-018: Incomplete Error Handling in util:format
**Severity:** 🟢 LOW  
**Location:** `util.erl:8`  
**Category:** Crash on Unregistered Process

**Description:**  
```erlang
[{registered_name, Name}] = process_info(Pid, [registered_name]),
```

This crashes if the process is not registered (returns `{registered_name, []}` or `undefined`).

**Impact:**
- Logging crashes for unregistered processes
- Error messages lost

**Suggested Fix:**
```erlang
Name = case process_info(Pid, registered_name) of
    {registered_name, RegName} when RegName =/= [] -> RegName;
    _ -> Pid
end,
```

---

## SECURITY & ROBUSTNESS ISSUES

### SEC-001: No Authentication/Authorization
**Severity:** 🟡 MEDIUM  
**Category:** Security

**Description:**  
The game server and clients have no authentication. Anyone can:
- Connect to any game
- Send arbitrary messages
- Impersonate other players

**Suggested Fix:**
Implement token-based authentication or shared secret.

---

### SEC-002: Binary Term Deserialization Attack Vector
**Severity:** 🟡 MEDIUM  
**Location:** `tcp_comm.erl:107`, `game_server.erl:93`  
**Category:** Security

**Description:**  
Using `binary_to_term/1` on untrusted network input can execute arbitrary code.

**Suggested Fix:**
Use `binary_to_term(Data, [safe])` to prevent code execution.

---

## PATTERNS LEADING TO FUTURE BUGS

### PATTERN-001: Overuse of Process Dictionary
**Description:**  
Extensive use of process dictionary makes code hard to reason about and test. State is hidden and can lead to subtle bugs.

**Locations:** Throughout codebase, especially game_manager and game_logic.

**Recommendation:**  
Refactor to use gen_server or gen_statem with explicit state records.

---

### PATTERN-002: No Supervision Trees
**Description:**  
Processes are spawned with `spawn` and `spawn_link` without proper supervision. When crashes occur, there's no automatic recovery.

**Recommendation:**  
Implement OTP supervision trees with proper restart strategies.

---

### PATTERN-003: Tight Coupling
**Description:**  
Modules directly call each other without interfaces, making testing and modification difficult.

**Recommendation:**  
Introduce behavior modules and dependency injection.

---

### PATTERN-004: Insufficient Logging
**Description:**  
While there are LOG macros, they're conditionally compiled out. In production, debugging issues would be nearly impossible.

**Recommendation:**  
Implement proper logging levels (debug, info, warning, error) using a logging framework.

---

### PATTERN-005: No Unit Tests
**Description:**  
No test suite exists for this complex distributed system.

**Recommendation:**  
Add EUnit tests for individual functions and Common Test for distributed scenarios.

---

## SUMMARY

### Bug Count by Severity:
- 🔴 **CRITICAL**: 5 bugs (can cause crashes or data corruption)
- 🟠 **HIGH**: 5 bugs (significant functional impact)
- 🟡 **MEDIUM**: 7 bugs (noticeable issues)
- 🟢 **LOW**: 3 bugs (minor issues)
- **Security**: 2 issues
- **Patterns**: 5 anti-patterns

**Total Issues Identified: 27**

### Priority Fixes (Recommended Order):
1. **BUG-004** - Fix crash in move_snakes (immediate crash risk)
2. **BUG-002** - Fix infinite reconnection loop (system hang)
3. **BUG-006** - Fix collision detection (game unplayable)
4. **BUG-001** - Fix race condition in player addition (state corruption)
5. **BUG-003** - Fix memory leaks (long-term stability)
6. **BUG-007** - Fix food generation infinite loop (game freeze)
7. **BUG-010** - Fix lock deadlock on node failure (permanent hang)
8. **BUG-005** - Fix message ordering (state inconsistency)

### Architectural Recommendations:
1. Migrate to OTP behaviors (gen_server, gen_statem, supervisor)
2. Replace process dictionary with explicit state management
3. Implement comprehensive error handling and recovery
4. Add supervision trees for fault tolerance
5. Create test suite for regression testing
6. Add configuration management system
7. Implement proper logging framework
8. Add monitoring and metrics collection

---

**Note:** This report documents functional bugs and runtime issues. Code style, naming conventions, and documentation quality issues were intentionally excluded as per the request to focus on "actual functional issues, logic errors, potential crashes, memory leaks, or other runtime problems."
