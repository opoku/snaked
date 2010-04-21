-module(game_manager).
-compile([export_all]).

start() ->
    spawn(game_manager, init, []).

init() ->

    MyId = one,
    DefaultPort = 5555,
    %%Invoke message passer.
    ok = message_passer:start(DefaultPort, [], MyId),

    %%Try to join an existing game.
    case join_game() of 
	%%If you joined a game, you will receive the game state from the current game.
	{ok, GameState, _HostList} -> 
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

broadcast_tick(Tick, NewFood) ->
    message_passer:broadcast({game_logic, {tick, Tick, NewFood}}).

stop() ->
    catch (clock:stop()),
    catch (game_logic:stop()),
    catch (message_passer:stop()).

