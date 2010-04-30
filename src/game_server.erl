%%% File    : game_server.erl
%%% Author  : ROAM - OA
%%% Description : A standalone server that tells clients which games are being played and also which nodes are playing
%%% Created :  8 Apr 2010 

-module(game_server).
-compile([export_all]).
-include("common.hrl").

-record(comm_state,
       {socket, game_list = [], gameid = 0}).


start() ->
    Port = get_server_port(),
    Pid = start_server(Port),
    register(game_server, Pid).
	%% start the backup servers
	primary_backup:start();

stop() ->
    connect_msg_disconnect("localhost", get_server_port(), {stop}),
    done.

reset() ->
    connect_msg_disconnect("localhost", get_server_port(), {reset}),
	%% reset backup servers
	primary_backup:reset();
    done.

debug() ->
    connect_msg_disconnect("localhost", get_server_port(), {debug, self()}),
    receive
	{game_server_debug, Result} ->
	    Result
    end.
	
connect_msg_disconnect(Host, Port, Msg) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet,0}]),
    game_server ! Msg,
    gen_tcp:close(Socket).

start_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,
					 {packet,0},
					 {reuseaddr, true},
					 {active, true}]),
    spawn(game_server, server_loop, [Listen, #comm_state{}]).

get_server_port() ->
    case get(server_port) of
	undefined ->
	    {Root, _Options} = filename:find_src(game_server),
	    PathToConfigFile = filename:absname_join(filename:dirname(Root), "../resources/server-config.txt"),
	    ?LOG("config file path: ~p~n", [PathToConfigFile]),
	    {ok, [{_ServerHost,ServerPort}, {_BackupHost, _BackupPort}]} = file:consult(PathToConfigFile),
	    put(server_port, ServerPort),
		ServerPort;
	ServerPort ->
	    ServerPort
    end.

server_loop(Listen, CommState) ->
    ?LOG("Waiting for connection on ~p~n",[Listen]),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    ?LOG("Connected on Socket ~p~n", [Socket]),
	    NewCommState = process_connection(Socket, CommState),
	    gen_tcp:close(Socket),
	    ?LOG("Socket ~p closed~n", [Socket]),

	    receive
		{reset} ->
		    ?LOG("Reseting server. Old state ~p~n", [NewCommState]),
		    server_loop(Listen, #comm_state{});
		{debug, Pid} ->
		    Pid ! {game_server_debug, NewCommState},
		    server_loop(Listen, NewCommState);
		{stop} ->
		    ?LOG("Stopping game server: Current state ~p~n", [NewCommState]),
		    gen_tcp:close(Listen)
	    after 0 ->
		    server_loop(Listen, NewCommState)
	    end;
	%% if accept fails
	{error, Reason} ->
	    Reason1 = Reason,
	    ?LOG("Error (~p)on listen Socket ~p. Relistening~n", [Reason1, Listen]),
	    server_loop(Listen, CommState)
    end.

process_connection(Socket, #comm_state{game_list = GameList, gameid = CurrentGameId} = CommState) ->
    receive
	{tcp, Socket, Data} ->
	    Data1 = binary_to_term(Data),
	    ?LOG("Received tcp data~p~n", [Data1]),
	    %% do something with received message
		%% forward the message to the primary backup
		forward_message_to_primary_backup(Data),
	    case Data1 of
		{get, game_list} ->
		    GameIdList = [{GameId, Name, length(NodeList)} || {GameId, Name, NodeList} <- GameList],
		    GameIdListData = term_to_binary({game_list, GameIdList}),
		    gen_tcp:send(Socket, GameIdListData),
		    CommState;
		%% send nodes in a game
		{get, game, GameId} ->
		    case lists:keyfind(GameId, 1, GameList) of
			{GameId, _Name, _NodeList} = GameInfo  ->
			    Bin = term_to_binary(GameInfo),
			    gen_tcp:send(Socket, Bin);
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}}))
		    end,
		    CommState;

		%% receive message from other nodes to add new nodes to a game and to
		%% start a new game
		{set, add_game, Name} ->
		    NextGameId = CurrentGameId + 1,
		    gen_tcp:send(Socket, term_to_binary({gameid, NextGameId})),
		    NewGameList = lists:keystore(NextGameId, 1, GameList, {NextGameId, Name, []}),
		    CommState#comm_state{gameid=NextGameId, game_list=NewGameList};

		{set, remove_game, GameId} ->
		    NewGameList = lists:keydelete(GameId, 1, GameList),
		    gen_tcp:send(Socket, term_to_binary({ok, game_removed})),
		    CommState#comm_state{game_list=NewGameList};
		%% the person who is responsible for adding the player sends this message
		{set, add_player, {GameId, {PlayerId, Ip, Port} = Player}} ->
		    Player1 = case Ip of
				  {127,0,0,1} -> % an ip of self means the game_server should figure out the ip
				      {ok, {HostIp, _Port}} = inet:peername(Socket),
				      {PlayerId, HostIp, Port};
				  Ip ->
				      Player
			      end,
		    case lists:keyfind(GameId, 1, GameList) of
			{GameId, Name, NodeList} ->
			    NewNodeList = [Player1 | NodeList],
			    NewGameList = lists:keystore(GameId, 1, GameList, {GameId, Name, NewNodeList}),
			    gen_tcp:send(Socket, term_to_binary({ok, player_added})),
			    CommState#comm_state{game_list=NewGameList};
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}})),
			    CommState
		    end;
		{set, remove_player, {GameId, {PlayerId, Ip, Port} = Player}} ->
		    Player1 = case Ip of
				  {127,0,0,1} -> % an localhost means the game_server should figure out the ip
				      {ok, {HostIp, _Port}} = inet:peername(Socket),
				      {PlayerId, HostIp, Port};
				  Ip ->
				      Player
			      end,
		    case lists:keyfind(GameId, 1, GameList) of
			{GameId, Name, NodeList} ->
			    Len1 = length(NodeList),
			    NewNodeList = NodeList -- [Player1],
			    Len2 = length(NewNodeList),
			    case Len2 of
				0 ->
				    %% no players left so just end the game
				    NewGameList = lists:keydelete(GameId, 1, GameList),
				    ?LOG("Player ~p removed and no players left so removing game ~p~n",
					 [PlayerId, GameId]),
				    gen_tcp:send(Socket, term_to_binary({ok, player_removed})),
				    CommState#comm_state{game_list=NewGameList};
				Len1 ->
				    %% nothing removed
				    gen_tcp:send(Socket, term_to_binary({error, {player_not_found, Player}})),
				    CommState;
				_Other ->
				    %% something happened
				    NewGameList = lists:keystore(GameId, 1, GameList, {GameId, Name, NewNodeList}),
				    ?LOG("Player ~p removed from game ~p~n", [PlayerId, GameId]),
				    gen_tcp:send(Socket, term_to_binary({ok, player_removed})),
				    CommState#comm_state{game_list=NewGameList}
			    end;
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}})),
			    CommState
		    end;
		Any ->
		    gen_tcp:send(Socket, term_to_binary({error, {invalid_message, Any}})),
		    CommState
	    end;
	{tcp_closed, Socket} ->
	    CommState
    end.
    

gs_client(Host, Port, Msg) ->
    case gen_tcp:connect(Host, Port, [binary, {packet,0}]) of,
    {ok, Socket} ->
		Bin = term_to_binary(Msg),
    	?LOG("Sending msg ~p to server~n", [Msg]),
    	Data = client_loop(Socket),
    	Term = binary_to_term(Data),
    	?LOG("Received ~p from server ~n", [Term]),
    	Term;
	{error, Reason} ->
		{error, message_resend}
	end.

client_loop(Socket) ->
    client_loop(Socket, []).

client_loop(Socket, DataList) ->
    receive
	{tcp, Socket, Data} ->
	    ?LOG("Received msg from server~n",[]),
	    client_loop(Socket, [Data | DataList]);
	{tcp_closed, Socket} ->
	    list_to_binary(lists:reverse(DataList))
    end.
	
start_gs_interface () ->
    nothing.

forward_message_to_backup_server(Msg) ->
    {BackupHost, BackupPort} = get_backup_info(),
    primary_backup:gs_client(BackupHost, BackupPort, Msg).

get_backup_info() ->
    case get(backup_info) of
	undefined ->
	    {Root, _Options} = filename:find_src(game_manager),
	    PathToConfigFile = filename:absname_join(filename:dirname(Root), "../resources/server-config.txt"),
	    ?LOG("config file path: ~p~n", [PathToConfigFile]),
	    {ok, [{ServerHost,ServerPort}, {BackupHost, BackupPort}]} = file:consult(PathToConfigFile),
	    BackupInfo = {BackupHost, BackupPort},
	    put(backup_info, BackupInfo),
	    BackupInfo;
	BackupInfo ->
	    BackupInfo
    end.

	
%% test_module() ->
%%     %% make sure server is fresh beforehand
%%     Tests = [{{get, game_list} , {game_list, []}},
%% 	     {{get, game, 0}, {error, {game_not_found, 0}}}
%% 	     ].


%%    {port, 0}, {server_name,"httpd_test"}, {server_root,"/tmp"}, {document_root,"/tmp/htdocs"}, {bind_address, "localhost"}
