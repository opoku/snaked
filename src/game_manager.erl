-module(game_manager).
-compile([export_all]).

-include("game_manager.hrl").


start() ->
	spawn(game_manager, init, []).

init() ->
	%Invoke message passer.
	ok = message_passer:start(5555, [], temp),
	
	%Try to join an existing game.
	case join_game() of 
		%If you joined a game, you will receive the game state from the current game.
		{ok, _GameState, _HostList} -> 
			done;
		%If you can't join the game, you start a new game and become a leader.
		fail -> 
			become_leader()
	end.

become_leader() ->
	clock:start().

join_game() ->
	fail.

broadcast_tick(Tick) ->
	message_passer:broadcast({game_logic, {tick, Tick}}).

		
	