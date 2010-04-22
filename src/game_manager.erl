-module(game_manager).
-compile([export_all]).

-define(MAX_PLAYERS, 8).

-record(manager_state, {nodeid, game_info, timeout = 5000, leader=false, leader_queue = queue:new()}).

%% TODO: race condition where the game_server's list of nodes is out of date

start(MyNodeId) ->
    spawn(game_manager, init, [MyNodeId]).

init(MyNodeId) ->
    put(id, MyNodeId),

    DefaultPort = 5555,
    %%Invoke message passer.
    
    message_passer:start(DefaultPort, MyNodeId, []),
    register(game_manager, self()),
    process_flag(trap_exit, true),

    %%Try to join an existing game.
    case join_game() of 
	{ok, {_GameId, _Name, _NodeList} = GameInfo, GameState} ->
	    %%If you joined a game, you will receive the game state from the current game.
	    game_logic:update_game_state(GameState),
		%% TODO: clarify if NodeId = SnakeId??
		%% TODO: create the leader queue based on the priority of the snakes in the snakeslists (sort snakelist)
		Snakes = GameState#game_state.snakes,
		SortedSnakes = lists:sort(fun compare_priority/2, Snakes),
		LeaderQueue = queue:from_list(SortedSnakes),
	    game_manager_loop(#manager_state{nodeid = MyNodeId, game_info=GameInfo, leader_queue = LeaderQueue});
	fail -> 
	    %%If you can't join the game, you start a new game and become a leader.
	    {_GameId, _Name, _NodeList} = GameInfo = create_new_game("DefaultName"),
		%% TODO: create a new leader queue!! add priority when you start the new game
		LeaderQueue = #manager_state.leader_queue,
		NewLeaderQueue = queue:in(MyNodeId, LeaderQueue),
	    %%start_game(MyNodeId),
	    io:format("Starting the game manager loop~n"),
	    game_manager_loop(#manager_state{nodeid=MyNodeId, game_info=GameInfo, leader=true, leader_queue = NewLeaderQueue})
    end.

create_new_game(Name) ->
    io:format("Creating a new game ~p~n", [Name]),
    MyNodeId = get(id),
    {gameid, GameId} = send_message_to_game_server({set, add_game, Name}),
    {HostIp, Port} = message_passer:get_host_info(MyNodeId),
    {ok, player_added} = send_message_to_game_server({set, add_player, {GameId, {MyNodeId, HostIp, Port}}}),
    {GameId, Name, [{MyNodeId, HostIp, Port}]}.
    
start_game(MyId) ->
    clock:start(),
    game_logic:start(MyId).

is_leader() ->
    game_manager ! {check_for_leader, self()},
    receive
	Result ->
	    Result
    end.

make_leader(NodeId) ->
    broadcast_to_mp({make_leader, NodeId}).

add_to_leader_queue(AddedSnakes) ->
	game_manager ! {add_to_leader_queue, AddedSnakes}.

update_leader_queue(LeaderQueue, {SnakeId, _Position, _Dir} | OtherAddedSnakes) ->
	NewLeaderQueue = queue:in(SnakeId, LeaderQueue),
	update_leader_queue(NewLeaderQueue, OtherAddedSnakes).
update_leader_queue(LeaderQueue, []) ->
	LeaderQueue.

compare_priority({_Id1, _Dir1, _Pos1, _Len1, _Score1, _Lives1, P1}, {_Id2, _Dir2, _Pos2, _Len2, _Score2, _Lives2, P2}) ->
	case P1 < P2 of
		true -> 
			true;
		false ->
			false
	end.

remove_from_leader_queue(SnakeId) ->
	game_manager ! {remove_from_leader_queue, SnakeId}.


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
    %% get game list
    %% select a game
    {game_list, GameList} = send_message_to_game_server({get, game_list}),
    select_next_game(GameList).

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
attempt_to_join_game({_GameId, _Name, NodeList} = GameInfo) ->
    NodeIdList = connect_to_nodes_in_list(NodeList),
    lists:foreach(fun message_passer:make_player/1, NodeIdList),
    message_passer:broadcast({game_manager, {hello, get(id)}}),
    join_loop(GameInfo, start).

connect_to_nodes_in_list(NodeList) ->
    [message_passer:connect(Host, Port) || {_PlayerId, Host, Port} <- NodeList]. 

select_next_game([{GameId, _Name, Length} = FirstGame| RestGames]) when Length < 8 ->
    %% now that we have selected the game, get information about the nodes from the server
    case send_message_to_game_server({get, game, GameId}) of
	%% could not find the game information on server
	{error, Reason} ->
	    io:format("Could not join game ~p : ~p", [FirstGame, Reason]),
	    select_next_game(RestGames);
	%% try to join the game
	GameInfo ->
	    io:format("Attempting to join game ~p~n", [GameId]),
	    attempt_to_join_game(GameInfo)
    end;
select_next_game([_FirstGame | RestOfGames]) ->
    select_next_game(RestOfGames);
select_next_game([]) ->
    io:format("No games left~n"),
    fail.

send_message_to_game_server(Msg) ->
    {ServerHost, ServerPort} = get_server_info(),
    game_server:gs_client(ServerHost, ServerPort, Msg).

get_server_info() ->
    case get(server_info) of
	undefined ->
	    {Root, _Options} = filename:find_src(game_manager),
	    PathToConfigFile = filename:absname_join(filename:dirname(Root), "../resources/server-config.txt"),
	    io:format("config file path: ~p~n", [PathToConfigFile]),
	    {ok, [ServerHost,ServerPort]} = file:consult(PathToConfigFile),
	    ServerInfo = {ServerHost, ServerPort},
	    put(server_info, ServerInfo),
	    ServerInfo;
	ServerInfo ->
	    ServerInfo
    end.

broadcast_tick(Tick) ->
    broadcast_tick(Tick, []).

broadcast_tick(Tick, Options) ->
    message_passer:broadcast({game_logic, {tick, Tick, Options}}).

stop() ->
    catch (clock:stop()),
    catch (game_logic:stop()),
    catch (message_passer:stop()).

start_game_manager(ManagerState) ->
    register(game_manager, self()),
    process_flag(trap_exit, true),
    game_manager_loop(ManagerState).

try_to_add_new_player(NodeId) ->
    PlayerCount = game_logic:count_players(),
    case PlayerCount < ?MAX_PLAYERS of
	true ->
	    game_manager ! {add_player, NodeId},
	    receive
		{player_added, NodeId} ->
		    io:format("Player ~p has been added so release the lock ~n", [NodeId]);
		{error, Reason} ->
		    io:format("Failed to add player: ~p~n", [Reason])
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
	    io:format("Received hi from ~p~n", [NodeId]),
	    game_logic:start(get(id)),
	    send_to_mp(NodeId, {join, get(id)}),
	    join_loop(GameInfo, {connected, NodeId})
    after
	%% 10000 ->
	infinity ->
	    fail
    end;
join_loop(GameInfo, {connected, NodeId}) ->
    receive
	{started, game_logic} ->
	    io:format("Received all the messages from nodes in the game~n"),
	    send_to_mp(NodeId, {joined, get(id)}),
	    join_loop(GameInfo, {joined, NodeId});
	{error, Reason} ->
	    io:format("Cannot join game: ~p~n", [Reason]),
	    fail
    end;
join_loop(GameInfo, {joined, _NodeId}) ->
    receive
	{game_state, GameState} ->
	    {ok, GameInfo, GameState}
    end.

game_manager_loop(#manager_state{nodeid = MyNodeId} = ManagerState) ->
    receive
	{hello, NodeId} ->
	    io:format("Received hello from ~p~n", [NodeId]),
	    send_to_mp(NodeId, {hi, MyNodeId}),
	    game_manager_loop(ManagerState);
	{join, NodeId} ->
	    io:format("Received join from ~p~n", [NodeId]),
	    Pid = spawn_link(fun () ->
				     message_passer:get_lock(add_player),
				     try try_to_add_new_player(NodeId)
				     after
					 message_passer:release_lock(add_player)
				     end
			     end),
	    put(NodeId, Pid),
	    game_manager_loop(ManagerState);
	{joined, NodeId} ->
	    io:format("Received joinED from ~p~n", [NodeId]),
	    GameState = game_logic:get_game_state(),
	    send_to_mp(NodeId, {game_state, GameState}),
	    %%release lock
	    game_manager_loop(ManagerState);
	{add_player, NodeId} ->
	    %% we can add the player
	    %% tell everyone to convert the player
	    message_passer:broadcast({make_player, NodeId}),

	    %% going to add the player to the game state with an empty position
	    message_passer:broadcast({game_logic, {add_player, NodeId}}),

	    game_manager_loop(ManagerState);
	{player_added, NodeId} ->
	    %% this is sent by the game logic when the player is added
	    Pid = get(NodeId),
	    Pid ! {player_added, NodeId},
	    erase(NodeId),

	    %% add the player to the game server
	    #manager_state{game_info={GameId, Name, NodeList}} = ManagerState,
	    HostInfo = message_passer:get_host_info(NodeId),
	    case send_message_to_game_server({set, add_player, {GameId, HostInfo}}) of
		{ok, player_added} ->
		    game_manager_loop(ManagerState#manager_state{game_info={GameId, Name, [HostInfo|NodeList]}});
		{error, Reason} ->
		    io:format("Add player failed ~p~n", [Reason]),
		    game_manager_loop(ManagerState)
	    end;
	{add_to_leader_queue, AddedSnakes} ->
		LeaderQueue = ManagerState#manager_state.leader_queue,
		NewLeaderQueue = update_leader_queue(LeaderQueue, AddedSnakes),
		game_manager_loop(ManagerState#manager_state{leader_queue = NewLeaderQueue});
	{check_for_leader, Pid} ->
	    Pid ! ManagerState#manager_state.leader,
	    game_manager_loop(ManagerState);
	{make_leader, MyNodeId} ->
	    %% If the nodeid is mine then make myself the leader
		game_manager_loop(ManagerState#manager_state{leader=true});
	{make_leader, OtherNodeId} ->
		%% if the nodeid is not mine then make sure that I'm not the leader
	    game_manager_loop(ManagerState#manager_state{leader=false});
	{remove_leader, MyNodeId} ->
	    %% this also makes sure that im not the leader
		%% remove myself from the leader queue
		game_manager_loop(ManagerState#manager_state{leader=false});
	{remove_from_leader_queue, NodeId} ->
		LeaderQueue = ManagerState#manager_state.leader_queue,
		case queue:head(LeaderQueue) of
			NodeId ->
				case queue:out(LeaderQueue) of
					{{value, SnakeId}, NewLeaderQueue} ->
						io:format("SnakeId removed from the head of leaderqueue ~p: ~p~n", [SnakeId, NewLeaderQueue]);
					{empty, LeaderQueue} ->
							io:format("empty leaderqueue ~p~n", [LeaderQueue]),
							NewLeaderQueue = LeaderQueue
				end,	
				%% send remove_leader and make_leader to self()
				Pid ! {remove_leader, NodeId},
				NewLeaderId = queue:head(NewLeaderQueue),
				Pid ! {make_leader, NewLeaderId};
				%% send remove_player to game_server : Ask Osei, where is the Player being created??
				send_message_to_game_server({set, remove_player, Player})
			OtherNodeId ->
				%% convert to queue to list, remove NodeId, and convert back to queue
				LeaderList = queue:to_list(LeaderQueue),
				NewLeaderList = lists:delete(NodeId, LeaderList),
				NewLeaderQueue = queue:from_list(NewLeaderList)
		end,
		game_manager_loop(ManagerState#manager_state.leader_queue = NewLeaderQueue);
	Any ->
	    io:format("Invalid message ~p~n", [Any]),
	    game_manager_loop(ManagerState)
    end.
