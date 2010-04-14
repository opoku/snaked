%%% File    : game_server.erl
%%% Author  : ROAM - OA
%%% Description : A standalone server that tells clients which games are being played and also which nodes are playing
%%% Created :  8 Apr 2010 

-module(game_server).
-compile([export_all]).

-record(comm_state,
       {socket, game_list = [], gameid = 0}).


start(Port) ->
    start_server(Port).

start_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,
					 {packet,0},
					 {reuseaddr, true},
					 {active, true}]),
    spawn(game_server, server_loop, [Listen, #comm_state{}]).

server_loop(Listen, CommState) ->
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

loop(Socket, #comm_state{game_list = GameList, gameid = CurrentGameId} = CommState) ->
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
			{GameId, _Name, _NodeList} = GameInfo  ->
			    Bin = term_to_binary(GameInfo),
			    gen_tcp:send(Socket, Bin);
			false ->
			    gen_tcp:send(Socket, term_to_binary({error, {game_not_found, GameId}}))
		    end,
		    NewCommState = CommState;

		%% receive message from other nodes to add new nodes to a game and to
		%% start a new game
		{set, add_game, Name} ->
		    NextGameId = CurrentGameId + 1,
		    gen_tcp:send(Socket, term_to_binary({gameid, NextGameId})),
		    NewGameList = lists:keystore(NextGameId, 1, GameList, {NextGameId, Name, []}),
		    NewCommState = CommState#comm_state{gameid=NextGameId, game_list=NewGameList};

		{set, remove_game, GameId} ->
		    NewGameList = lists:keydelete(GameId, 1, GameList),
		    gen_tcp:send(Socket, term_to_binary({ok, game_removed})),
		    NewCommState = CommState#comm_state{game_list=NewGameList};
		%% the person who is responsible for adding the player sends this message
		{set, add_player, {GameId, {_PlayerId, _Ip, _Port} = Player}} ->
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
		{set, remove_player, {GameId, {_PlayerId, _Ip, _Port} = Player}} ->
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
		Any ->
		    gen_tcp:send(Socket, term_to_binary({error, {invalid_message, Any}})),
		    NewCommState = CommState
	    end,
	    gen_tcp:close(Socket),
	    io:format("Socket ~p closed~n", [Socket]),
	    NewCommState
    end.
    

gs_client(Host, Port, Msg) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet,0}]),
    Bin = term_to_binary(Msg),
    io:format("Sending msg ~p to server~n", [Msg]),
    gen_tcp:send(Socket, Bin),
    Data = client_loop(Socket),
    Term = binary_to_term(Data),
    io:format("Received ~p from server ~n", [Term]),
    Term.

client_loop(Socket) ->
    client_loop(Socket, []).

client_loop(Socket, DataList) ->
    receive
	{tcp, Socket, Data} ->
	    io:format("Received msg from server~n"),
	    client_loop(Socket, [Data | DataList]);
	{tcp_closed, Socket} ->
	    list_to_binary(lists:reverse(DataList))
    end.
	
	
test_module() ->
    %% make sure server is fresh beforehand
    Tests = [{{get, game_list} , {game_list, []}},
	     {{get, game, 0}, {error, {game_not_found, 0}}}
	     ].
