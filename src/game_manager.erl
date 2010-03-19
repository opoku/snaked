-module(game_manager).
-compile([export_all]).

start() ->
    spawn(game_manager, init, []).

init() ->

    MyId = temp,
    DefaultPort = 5555,
    %%Invoke message passer.
    ok = message_passer:start(DefaultPort, [], MyId),

    %%Try to join an existing game.
    case join_game() of 
	%%If you joined a game, you will receive the game state from the current game.
	{ok, _GameState, _HostList} -> 
	    done;
	%%If you can't join the game, you start a new game and become a leader.
	fail -> 
	    become_leader(MyId)
    end.

become_leader(MyId) ->
    clock:start(),
    game_logic:start(MyId).

join_game() ->
    fail.

broadcast_tick(Tick) ->
    message_passer:broadcast({game_logic, {tick, Tick}}).



