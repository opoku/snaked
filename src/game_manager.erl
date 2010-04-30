-module(game_manager).
-compile([export_all]).
-include("common.hrl").

-define(MAX_PLAYERS, 8).

-include("game_state.hrl").
-record(host_info, {id, priority}).
-record(manager_state, {nodeid, game_info, timeout = 5000, leader=false, leader_queue = queue:new()}).

%% TODO: race condition where the game_server's list of nodes is out of date

start([MyNodeId, Port]) ->
    MyNodeIdAtom = list_to_atom(MyNodeId),
    PortNum = list_to_integer(Port),
    start(MyNodeIdAtom, PortNum).

start(MyNodeId, Port) ->
    Pid = spawn(game_manager, init, [MyNodeId, Port]),
    register(game_manager, Pid).

debug() ->
    game_manager ! {debug, self()},
    receive
	{game_manager, Any} ->
	    Any
    end.

game_monitor(GameId, NodeId, Port) ->
    MonRef = erlang:monitor(process, game_manager),
    receive
	{'DOWN', MonRef, process, _Name, Info } ->
	    ?LOG("Game manager down: ~p.  Removing from game_server~n", [Info]),
	    send_message_to_game_server({set, remove_player, {GameId, {NodeId, {127,0,0,1}, Port}}})
    end.

init(MyNodeId, DefaultPort) ->
    put(id, MyNodeId),
    put(defaultport, DefaultPort),
    %%DefaultPort = 5555,
    %%Invoke message passer.

    message_passer:start(DefaultPort, MyNodeId, []),
    process_flag(trap_exit, true),

    %%Try to join an existing game.
    case join_game() of 
	{ok, {GameId, _, _} = GameInfo, GameState} ->
	    %%If you joined a game, you will receive the game state from the current game.
	    spawn(game_manager, game_monitor, [GameId, MyNodeId, DefaultPort]),
	    game_logic:update_game_state(GameState),
	    clock:start(),
	    game_manager_loop(#manager_state{nodeid = MyNodeId, game_info=GameInfo});

	fail -> 
	    %%If you can't join the game, you start a new game and become a leader.
	    {GameId, _Name, NodeList} = GameInfo = create_new_game("DefaultName"),
	    spawn(game_manager, game_monitor, [GameId, MyNodeId, DefaultPort]),

	    clock:start(),
	    game_logic:start(MyNodeId, [NodeId || #host_info{id=NodeId} <- NodeList]),
	    game_logic:start_game(),

	    ?LOG("Starting the game manager loop~n",[]),
	    game_manager_loop(#manager_state{nodeid=MyNodeId, game_info=GameInfo, leader=true})
    end.

create_new_game(Name) ->
    ?LOG("Creating a new game ~p~n", [Name]),
    MyNodeId = get(id),
    {gameid, GameId} = send_message_to_game_server({set, add_game, Name}),
    add_player_to_game_server(GameId, MyNodeId),
    message_passer:make_player(MyNodeId),
    {GameId, Name, [#host_info{id=MyNodeId, priority=1}]}.
    
is_leader() ->
    game_manager ! {check_for_leader, self()},
    receive
	{game_manager, is_leader_result, Result} ->
	    ?LOG("leader result: ~p~n", [Result]),
	    Result
    end.

make_leader(NodeId) ->
    broadcast_to_mp({make_leader, NodeId}).

add_to_leader_queue(AddedSnakes) ->
	broadcast_to_mp({add_to_leader_queue, AddedSnakes}).

update_leader_queue(LeaderQueue, [#snake{id=SnakeId} | OtherAddedSnakes]) ->
	NewLeaderQueue = queue:in(SnakeId, LeaderQueue),
	update_leader_queue(NewLeaderQueue, OtherAddedSnakes);
update_leader_queue(LeaderQueue, []) ->
	LeaderQueue.

remove_from_game_info(SnakeId) ->
    game_manager ! {remove_from_game_info, self(), SnakeId},
    receive
	{game_manager, removed_from_game_info, SnakeId} ->
	    ok;
	{game_manager, error, Reason, SnakeId} ->
	    ?LOG("Error removing ~p : ~p", [Reason, SnakeId]),
	    error
    end.

join_game() ->
    %% get game list
    %% select a game
    {game_list, GameList} = send_message_to_game_server({get, game_list}),
    select_next_game(GameList).

attempt_to_join_game({GameId, Name, NodeList}) ->
    NodeIdList = connect_to_nodes_in_list(NodeList),
    lists:foreach(fun message_passer:make_player/1, NodeIdList),
    broadcast_to_mp({hello, get(id)}),
    join_loop({GameId, Name, NodeIdList}, start).

connect_to_nodes_in_list(NodeList) ->
    [message_passer:connect(Host, Port) || {_PlayerId, Host, Port} <- NodeList]. 

select_next_game([{GameId, _Name, Length} = FirstGame| RestGames]) when Length < 8 ->
    %% now that we have selected the game, get information about the nodes from the server
    case send_message_to_game_server({get, game, GameId}) of
	%% could not find the game information on server
	{error, Reason} ->
	    ?LOG("Could not join game ~p : ~p", [FirstGame, Reason]),
	    select_next_game(RestGames);
	%% try to join the game
	GameInfo ->
	    ?LOG("Attempting to join game ~p~n", [GameId]),
	    attempt_to_join_game(GameInfo)
    end;
select_next_game([_FirstGame | RestOfGames]) ->
    select_next_game(RestOfGames);
select_next_game([]) ->
    ?LOG("No games left~n",[]),
    fail.

add_player_to_game_server(GameId, NodeId) ->
    HostInfo = message_passer:get_host_info(NodeId),
    case send_message_to_game_server({set, add_player, {GameId, HostInfo}}) of
	{ok, player_added} ->
	    ?LOG("Adding player to game server succeeded~n",[]);
	{error, Reason} ->
	    ?LOG("Add player failed ~p~n", [Reason])
    end.

remove_player_from_game_server(GameId, NodeId) ->
    ?LOG("Removing player ~p from game server", [NodeId]),
    HostInfo = message_passer:get_host_info(NodeId),
    case send_message_to_game_server({set, remove_player, {GameId, HostInfo}}) of
	{ok, player_removed} ->
	    ?LOG("Removed player from game server succeeded~n",[]);
	{error, Reason} ->
	    ?LOG("Remove player failed ~p~n", [Reason])
    end.


send_message_to_game_server(Msg) ->
    {ServerHost, ServerPort} = get_server_info(),
    game_server:gs_client(ServerHost, ServerPort, Msg).

get_server_info() ->
    case get(server_info) of
	undefined ->
	    {Root, _Options} = filename:find_src(game_manager),
	    PathToConfigFile = filename:absname_join(filename:dirname(Root), "../resources/server-config.txt"),
	    ?LOG("config file path: ~p~n", [PathToConfigFile]),
	    {ok, [ServerHost,ServerPort]} = file:consult(PathToConfigFile),
	    ServerInfo = {ServerHost, ServerPort},
	    put(server_info, ServerInfo),
	    ServerInfo;
	ServerInfo ->
	    ServerInfo
    end.

add_player_to_game_info(NodeId) ->
    game_manager ! {add_to_game_info, NodeId},
    done.

broadcast_tick(Tick) ->
    broadcast_tick(Tick, []).

broadcast_tick(Tick, Options) ->
    message_passer:broadcast({game_logic, {tick, Tick, Options}}).

stop() ->
    catch (clock:stop()),
    catch (game_logic:stop()),
    catch (message_passer:stop()).

try_to_add_new_player(NodeId) ->
    PlayerCount = game_logic:count_players(),
    case PlayerCount < ?MAX_PLAYERS of
	true ->
	    %% send a message telling the new player that he is being added so that he
	    %% starts his gamelogic.
	    send_to_mp(NodeId, {adding, get(id)}),
	    ok = receive
		     {ok, NodeId} ->
			 ok
		 end,
	    game_manager ! {add_player, NodeId},
	    receive
		{player_added, NodeId} ->
		    ?LOG("Player ~p has been added so release the lock ~n", [NodeId]);
		{error, Reason} ->
		    ?LOG("Failed to add player: ~p~n", [Reason])
	    end;
	false ->
	    %% cannot add player
	    send_to_mp(NodeId, {error, game_full})
    end.

send_to_mp(NodeId, Msg) ->
    message_passer:unicast(NodeId, {game_manager, Msg}).

broadcast_to_mp(Msg) ->
    message_passer:broadcast({game_manager, Msg}).

join_loop(GameInfo, start) ->
    receive
	{hi, NodeId} ->
	    ?LOG("Received hi from ~p~n", [NodeId]),
	    send_to_mp(NodeId, {join, get(id)}),
	    join_loop(GameInfo, {connected, NodeId})
    after
	%% 10000 ->
	infinity ->
	    fail
    end;
join_loop(GameInfo, {connected, NodeId}) ->
    receive
	{adding, NodeId} ->
	    {_,_,NodeList}= GameInfo,
	    game_logic:start(get(id), [NodeId1 || #host_info{id=NodeId1} <- NodeList]),
	    message_passer:make_player(get(id)),
	    send_to_mp(NodeId, {ok, get(id)}),
	    join_loop(GameInfo, {connected, NodeId});
	{started, game_logic} ->
	    ?LOG("Received all the messages from nodes in the game~n",[]),
	    send_to_mp(NodeId, {joined, get(id)}),
	    join_loop(GameInfo, {joined, NodeId});
	{error, Reason} ->
	    ?LOG("Cannot join game: ~p~n", [Reason]),
	    fail
    end;
join_loop(_GameInfo, {joined, _NodeId}) ->
    receive
	{game_state, GameState, NewGameInfo} ->
	    {ok, NewGameInfo, GameState}
    end.

find_max_priority(NodeList) ->
    lists:max([Priority || #host_info{priority=Priority} <- NodeList]).

update_node_list(P, [#host_info{priority=P1}=Host | NodeList]) when P < P1 ->
    [Host#host_info{priority=P1-1} | update_node_list(P, NodeList)];
update_node_list(P, [Host | NodeList]) ->
    [Host | update_node_list(P, NodeList)];
update_node_list(_, []) ->
    [].

game_manager_loop(#manager_state{nodeid = MyNodeId} = ManagerState) ->
    receive
	{hello, NodeId} ->
	    ?LOG("Received hello from ~p~n", [NodeId]),
	    send_to_mp(NodeId, {hi, MyNodeId}),
	    game_manager_loop(ManagerState);
	{join, NodeId} ->
	    ?LOG("Received join from ~p~n", [NodeId]),
	    Pid = spawn_link(fun () ->
				     put(id, MyNodeId),
				     message_passer:get_lock(add_player),
				     try try_to_add_new_player(NodeId)
				     after
					 message_passer:release_lock(add_player)
				     end
			     end),
	    put(NodeId, Pid),
	    game_manager_loop(ManagerState);
	{ok, NodeId} ->
	    case get(NodeId) of
		undefined ->
		    game_manager_loop(ManagerState);
		Pid ->
		    Pid ! {ok, NodeId},
		    game_manager_loop(ManagerState)
	    end;
	{joined, NodeId} ->
	    ?LOG("Received joinED from ~p~n", [NodeId]),
	    GameInfo = ManagerState#manager_state.game_info,
	    spawn(fun () ->
			  GameState = game_logic:get_game_state(),
			  send_to_mp(NodeId, {game_state, GameState, GameInfo})
		  end),
	    %%release lock
	    game_manager_loop(ManagerState);
	{add_player, NodeId} ->
	    %% we can add the player
	    %% going to add the player to the game state with an empty position
	    message_passer:broadcast({game_logic, {add_player, NodeId, get(id)}}),

	    %% create a checklist for acks
	    #manager_state{game_info = {_,_,NodeList}} = ManagerState,
	    IdList = [Id || #host_info{id=Id} <- NodeList],
	    put({NodeId, addplayer},IdList),

	    game_manager_loop(ManagerState);
	{add_to_game_info, NodeId} ->
	    %% adding this nodeid to game info and also generates the priority of that new node
	    #manager_state{game_info={GameId, Name, NodeList}} = ManagerState,
	    NewPriority = find_max_priority(NodeList) + 1,
	    ?LOG("adding a player ~p to nodelist ~p with priority ~p~n", [NodeId, NodeList, NewPriority]),
	    game_manager_loop(ManagerState#manager_state{game_info={GameId, Name,
								    [#host_info{id=NodeId, priority=NewPriority}|NodeList]}});
	{player_added, NodeId, AckSenderId} ->
	    %% make sure you receive acks from everyone else in the game
	    IdList = get({NodeId, addplayer}),
	    IdList1 = case IdList of
			  undefined ->
			      undefined;
			  IdList -> 
			      IdList -- [AckSenderId]
		      end,
	    ?LOG("Remaining  player_added acks expected for ~p : ~p~n", [NodeId, IdList1]),
	    case IdList1 of
		undefined ->
		    game_manager_loop(ManagerState);
		[] ->

		    ?LOG("Received all player_added acks so adding ~p to game server~n",[NodeId]),
		    erase({NodeId, addplayer}),

		    %% tell background process that player has been added
		    Pid = get(NodeId),
		    Pid ! {player_added, NodeId},
		    erase(NodeId),

		    
		    %% add the player to the game server
		    {GameId,_,_} = ManagerState#manager_state.game_info,
		    spawn(fun () -> add_player_to_game_server(GameId, NodeId) end),
		    game_manager_loop(ManagerState);
		_Any ->
		    put({NodeId, addplayer}, IdList1),
		    game_manager_loop(ManagerState)
	    end;
	{check_for_leader, Pid} ->
	    Pid ! {game_manager, is_leader_result, ManagerState#manager_state.leader},
	    game_manager_loop(ManagerState);
	{make_leader, MyNodeId} ->
	    %% If the nodeid is mine then make myself the leader
	    ?LOG("I have been made the leader~n",[]),
	    game_manager_loop(ManagerState#manager_state{leader=true});
	{make_leader, OtherNodeId} ->
	    %% if the nodeid is not mine then make sure that I'm not the leader
	    ?LOG("I am making someone else ~p a leader~n",[OtherNodeId]),
	    game_manager_loop(ManagerState#manager_state{leader=false});
	{remove_leader, MyNodeId} ->
	    %% this also makes sure that im not the leader
	    %% remove myself from the leader queue
	    game_manager_loop(ManagerState#manager_state{leader=false});
	{remove_from_game_info, Pid, NodeId} ->
	    %% the game info holds the leader queue
	    %% removes the dead node from the leader queue
	    ?LOG("Removing ~p from game_info~n", [NodeId]),
	    {GameId, Name, NodeList} = ManagerState#manager_state.game_info,
	    case lists:keytake(NodeId, #host_info.id, NodeList) of
		{value, #host_info{priority=NodePriority}, RestOfNodeList} ->
		    %% this function will update the other node priorities accordingly
		    spawn(fun () -> remove_player_from_game_server(GameId, NodeId) end),
		    Rest1 = update_node_list(NodePriority, RestOfNodeList),
		    case {NodePriority, Rest1} of
			{1, []} ->
			    ?LOG("We were the only one so just continue~n", []),
			    %% stop my clock
			    clock:stop();
			{1, Rest1} ->
			    %% someone else must now be one
			    ?LOG("We have a new leader.  new NodeList: ~p~n", [Rest1]),
			    #host_info{id=NewLeaderId} = lists:keyfind(1, #host_info.priority, Rest1),
			    game_manager ! {make_leader, NewLeaderId};
			_Other ->
			    ?LOG("No new leader.  new NodeList: ~p~n", [Rest1])
		    end,
		    NewGameInfo = {GameId, Name, Rest1},
		    Pid ! {game_manager, removed_from_game_info, NodeId},
		    game_manager_loop(ManagerState#manager_state{game_info=NewGameInfo});
		false ->
		    ?LOG("Cannot find node ~p in NodeList~n", [NodeId]),
		    Pid ! {game_manager, error, node_not_found, NodeId},
		    game_manager_loop(ManagerState)
	    end;
	{debug, Pid} ->
	    Pid ! {game_manager, ManagerState},
	    game_manager_loop(ManagerState);
	{'EXIT', Pid, Reason} ->
	    ?LOG("Process ~p died : ~p~n", [Pid, Reason]),
	    case whereis(game_logic) of
		undefined -> %% game_logic is dead
		    message_passer:stop(),
		    clock:stop(),
		    ?LOG("I am dying~n",[]);
		_Else ->
		    game_manager_loop(ManagerState)
	    end;
	{restart} ->
	    game_logic:stop(),
	    message_passer:stop(),
	    game_manager:init(get(id), get(defaultport));
	Any ->
	    ?LOG("Invalid message ~p~n", [Any]),
	    game_manager_loop(ManagerState)
    end.
