-module(game_manager).
-compile([export_all]).

-include("game_defs.hrl").

-record(manager_state, {gameid}).

%% TODO: race condition where the game_server's list of nodes is out of date

start() ->
    spawn(game_manager, init, []).

init() ->

    MyId = one,
    DefaultPort = 5555,
    %%Invoke message passer.
    ok = message_passer:start(DefaultPort, [], MyId),

    spawn(game_manager, start_game_manager, []),

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

broadcast_tick(Tick, NewFood) ->
    message_passer:broadcast({game_logic, {tick, Tick, NewFood}}).

stop() ->
    catch (clock:stop()),
    catch (game_logic:stop()),
    catch (message_passer:stop()).


start_game_manager() ->
    register(game_manager, self()),
    process_flag(trap_exit, true),
    game_manager_loop(#manager_state{}).

try_to_add_new_player(NodeId) ->
    message_passer:get_lock(add_player),
    PlayerCount = game_logic:count_players(),
    case PlayerCount < MAX_PLAYERS of
	true ->
	    %% we can add the player
	    %% tell everyone to convert the player
	    message_passer:broadcast({make_player, NodeId}),

	    %% going to add the player to the game state with an empty position
	    message_passer:broadcast({game_logic, {add_player, NodeId}});
	false ->
	    %% cannot add player
	    message_passer:unicast(NodeId, {error, game_full})
    end

game_manager_loop(ManagerState) ->
    receive
	{hello, NodeId} ->
	    io:format("Received hello from ~p~n", [NodeId]),
	    #manager_state{myid=MyId} = ManagerState,
	    message_passer:unicast(NodeId, {hi, MyId}),
	    game_manager_loop(ManagerState);
	{join, NodeId} ->
	    io:format("Received join from ~p~n", [NodeId]),
	    spawn_link(fun () -> try_to_add_new_player(NodeId) end),
	    game_manager_loop(ManagerState);
	{joined, NodeId} ->
	    io:format("Received joinED from ~p~n", [NodeId]),
	    GameState = game_logic:get_game_state(),
	    message_passer:unicast(NodeId, {game_logic, {add_game_state, GameState}}),
	    %%release lock
	    game_manager_loop(ManagerState);
	Any ->
	    io:format("Invalid message ~p~n", [Any]),
	    game_manager_loop(ManagerState)
    end.
