%%% File    : game_server.erl
%%% Author  : ROAM - OA
%%% Description : A standalone server that tells clients which games are being played and also which nodes are playing
%%% Created :  8 Apr 2010 

-module(game_server).
-compile([export_all]).

-record(comm_state,
       {socket}).


start(Port) ->
    start_server(Port).

start_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,
					 {packet,0},
					 {reuseaddr, true},
					 {active, true}]),
    spawn(tcp_comm, server_loop, [Port, Listen, #comm_state{}]).

server_loop(Listen, #comm_state{game_list=GameList} = CommState) ->
    io:format("Waiting for connection~n"),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    io:format("Connected on Socket ~p~n", [Socket]),
	    NewCommState = loop(Socket, CommState),
	    server_loop(Listen, NewCommState);
	%% if accept fails
	{error, Reason} ->
	    io:format("Error on listen Socket ~p(~p). Not relistening~n", [Listen, Reason]),
	    server_loop(Listen, CommState)
    end.

loop(Socket, CommState) ->
    receive
	{tcp, Socket, Data} ->
	    Data1 = binary_to_term(Data),
	    io:format("Received tcp data~p~n", [Data1]),
	    %% do something with received message
	    case Data1 of
		{get, game_list} ->
		    GameIdList = [{GameId, length(NodeList)} || {GameId, _, NodeList} <- GameList],
		    GameIdListData = term_to_binary({game_list, GameIdList}),
		    gen_tcp:send(Socket, GameIdListData),
		    NewCommState = CommState;
		%% send nodes in a game
		{get, game, GameId} ->
		    case lists:keyfind(GameId, 1, GameList) of
			{GameId, Name, NodeList} = GameInfo  ->
			    Bin = term_to_binary(GameInfo),
			    gen_tcp:send(Socket, Bin);
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}})),
		    end,
		    NewCommState = CommState;

		%% receive message from other nodes to add new nodes to a game and to
		%% start a new game
		{set, add_game, Name} ->
		    NextGameId = GameId + 1,
		    gen_tcp:send(Socket, term_to_binary({gameid, NextGameId})),
		    NewGameList = lists:keystore(NextGameId, 1, GameList, {NextGameId, Name, []}),
		    NewCommState = CommState#comm_state{gameid=NextGameId, game_list=NewGameList};

		{set, remove_game, GameId} ->
		    NewGameList = lists:keydelete(GameId, 1, GameList),
		    gen_tcp:send(Socket, term_to_binary({ok, game_removed})),
		    NewCommState = CommState#comm_state{game_list=NewGameList};
		%% the person who is responsible for adding the player sends this message
		{set, add_player, {GameId, {PlayerId, Ip, Port} = Player}} ->
		    case lists:keyfind(GameId, 1, GameList) of
			{GameId, Name, NodeList} ->
			    NewNodeList = [Player | NodeList],
			    NewGameList = lists:keystore(GameId, 1, GameList, {GameId, Name, NewNodeList}),
			    gen_tcp:send(Socket, term_to_binary({ok, player_added})),
			    NewCommState = CommState#comm_state{game_list=NewGameList};
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}})),
			    NewCommState = CommState
		    end;
		{set, remove_player, {GameId, {PlayerId, Ip, Port} = Player}} ->
		    case lists:keyfind(GameId, 1, GameList) of
			{GameId, Name, NodeList} ->
			    NewNodeList = NodeList -- [Player],
			    NewGameList = lists:keystore(GameId, 1, GameList, {GameId, Name, NewNodeList}),
			    gen_tcp:send(Socket, term_to_binary({ok, player_removed})),
			    NewCommState = CommState#comm_state{game_list=NewGameList};
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}})),
			    NewCommState = CommState
		    end;
		_Any ->
		    NewCommState = CommState
	    end,
	    gen_tcp:close(Socket),
	    server_loop(Listen, NewCommState);

	{tcp_closed, Socket} ->
	    io:format("Socket ~p closed~n", [Socket]),
	    CommState
    end.
    


%% each thing in gamelist is a tuple {gameid, name, list of node/port}

comm_loop(#comm_state{socket=Socket, game_list=GameList} = CommState) ->
    receive
	{tcp, Socket, Data} ->
	    %% forward to message passer
	    Data1 = binary_to_term(Data),
	    io:format("Received tcp data~p~n", [Data1]),

	    %% do something with received message
	    case Data of
		{get, game_list} ->
		    GameIdList = [{GameId, length(NodeList)} || {GameId, _, NodeList} <- GameList],
		    GameIdListData = term_to_binary(GameIdList),
		    gen_tcp:send(Socket, GameIdListData),
		    comm_loop(CommState);
		%% send nodes in a game
		{get, game, GameId} ->
		    case lists:keyfind(GameId, 1, GameList) of
			{GameId, Name, NodeList} = GameInfo  ->
			    Bin = term_to_binary(GameInfo),
			    gen_tcp:send(Socket, Bin),
			    comm_loop(CommState);
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}})),
			    comm_loop(CommState)
		    end;

		%% receive message from other nodes to add new nodes to a game and to
		%% start a new game
		{set, } ->
	    end;

	    comm_loop(CommState);
	{tcp_closed, Socket} ->
	    io:format("Socket ~p closed~n", [Socket]),
	    exit(socketclosed)
    end.
