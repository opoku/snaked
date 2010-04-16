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

%%Newbie
%%-------
%%
%%1. get game_list
%%2. provide a choise to user
%%   (b) user selects a game
%%3. get, game, GameId
%%4. a list of nodes
%%5. broadcast to players (HELLO)
%%6. wait for first reply (HI)
%%   - disregard all other HIs
%%7. unicast JOIN to handler
%%8. if you can join
%%   - you will start recving tick and event updates from players in group
%%   - when you have received events from all players in the game, send a (JOINED) to handler, then handler will reply with the game state
%%  
%%  else
%%   you will receive (error, game_full)
%%   close connections to node list
%%9. try another game (back to 1)
%%   or start a new game
%%
%% @ret should return fail if it fails
join_game() ->
    {ServerHostAddress, ServerPort} = get_server_details(),
    %% get game list
    GameList = send_message_to_game_server(ServerHostAddress, ServerPort, {get, game_list}),
    %% select a game
    case GameList of
        %% there are no games going on, so cannot join the game.
        [] ->
            fail;
        %% there are some games going on
        %% TODO: present a UI to user to let him select the game
        %% for now, we select the first game with length < 8.
        GameList2 ->
            SelectedGameId = select_game(GameList2),
            case SelectedGameId of
                fail ->
                    fail;
                SelectedGame ->
                    %% now that we have selected the game, get information about the nodes from the server
                    GameInfo = send_message_to_game_server(ServerHostAddress, ServerPort, {get, game, SelectedGame}),
                    case GameInfo of
                        %% could not find the game information on server
                        {error, _Any} ->
                            fail;
                        %% try to join the game
                        {GameId, Name, NodeList} ->
                            case attempt_to_join_game(GameInfo) of
                                fail ->
                                    fail;
                                %% return game state and host list
                                {GameState, HostList} ->
                                    {ok, GameState, HostList}
                            end
                    end
            end
    end.

%%5. broadcast to players (HELLO)
%%6. wait for first reply (HI)
%%   - disregard all other HIs
%%7. unicast JOIN to handler
%%8. if you can join
%%   - you will start recving tick and event updates from players in group
%%   - when you have received events from all players in the game, send a (JOINED) to handler, then handler will reply with the game state
%%  
%%  else
%%   you will receive (error, game_full)
%%   close connections to node list
attempt_to_join_game(GameInfo) ->
    {GameId, Name, NodeList} = GameInfo,
    fail.

select_game([H|T]) ->
    {GameID, Name, Length} = H,
    case Length < 8 of
    true ->
        H;
    false ->
        select_game(T)
    end;
select_game([]) ->
    fail.

send_message_to_game_server(ServerHostAddress, ServerPort, Msg) ->
    game_server:gs_client(ServerHostAddress, ServerPort, Msg).

get_server_details() ->
    {Root, _Options} = filename:find_src(game_logic),
    PathToConfigFile = filename:absname_join(filename:dirname(Root), "../resources/config.txt"),
    io:format("config file path: ~p~n", [PathToConfigFile]),
    {ok, [Configurations]} = file:consult(PathToConfigFile),
    [ServerHostAddress|T] = Configurations,
    [ServerPort|T2] = T,
    {ServerHostAddress, ServerPort}.
    
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
