-module(message_passer).
-compile([export_all]).

% message passer does two things
% 1. sends messages over tcp/udp to specific host/multicasts to all 
% 2. receives a messages of tcp/udp and then route the message to game_logic or game_manager
% 3. locks

% the game_logic and game_manager processes are registered processes

server() ->
    server(0, []).

server(HostList) ->
    server(0, HostList).

server(Port, HostList) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    register(message_passer, self()),
    loop(Socket, [{Id, hostname_to_ip(Host), Port1} || {Id, Host, Port1} <- HostList]).

usend(Socket, Id, Msg, HostList) ->
    {ok, {Id, Host, Port}} = find_host(Id, HostList),
    ok = gen_udp:send(Socket, Host, Port, Msg).

hostname_to_ip(HostName) ->
    {ok, Ip} = inet:getaddr(HostName, inet),
    Ip.
    
bsend(Socket, Msg, HostList) ->
    lists:foreach(fun ({_, Host, Port}) -> gen_udp:send(Socket, Host, Port, Msg) end, HostList).

find_host(Id, [{Id, Host, Port} | _]) -> {ok, {Id, Host, Port}};
find_host(Id, [ _ | HostList]) -> find_host(Id, HostList);
find_host(_, []) -> not_found.

find_id({Host, Port}, [{Id, Host, Port} | _HostList]) -> {ok, {Id, Host, Port}};
find_id(Key, [ _ | HostList]) -> find_id(Key, HostList);
find_id(_, []) -> not_found.

increment(Msg) -> 
          MsgId = term_to_binary[Msg]+1.

route_message(Host, Port, Msg, HostList) ->
    {ok , {Id, Host, Port}} = find_id({Host, Port}, HostList),
    io:format("Route Message ~p from ~p~n", [Msg, Id]),
    case element(1, Msg) of
	rmulti ->
	    message_passer ! Msg


	    
%% 	game_logic ->
%% 	    game_logic ! Msg;
%% 	game_manager ->
%% 	    game_manager ! Msg;
    end.

loop(Socket, HostList) ->
    receive
	{udp, Socket, Host, Port, Msg} ->
	    io:format("Received udp message from ~p : ~p~n", [{Host, Port}, Msg]),
	    route_message(Host, Port, binary_to_term(Msg), HostList),
	    loop(Socket, HostList);
	{unicast, Id, Msg} ->
	    io:format("Sending unicast message to id ~p : ~p~n", [Id, Msg]),
	    usend(Socket, Id, term_to_binary(Msg), HostList),
	    loop(Socket, HostList);
	{broadcast, Msg} ->
	    io:format("Sending broadcast message : ~p~n", [Msg]),
	    bsend(Socket, term_to_binary(Msg), HostList),
	    loop(Socket, HostList);
	{multicast, Msg} ->
	    %% reliable multicast
	    io:format("Sending multicast message : ~p~n", [Msg]),
		%%increment(Msg),
	    bsend(Socket, term_to_binary({rmulti, increment(Msg), Msg}), HostList),
	    loop(Socket, HostList);
	{rmulti, MsgId, Msg} ->
%% On receive {rmulti, MsgId, Msg}:
%% 	   Create Acklist [{ack, NodeId_1, MsgId}, ... ]
%% 	   bsend({ack, MyNode, Msgid})
       AckList = [{ack,NodeId,MsgId} || {NodeId,Ip,Port} -> HostList ],
       bsend(Socket,term_to_binary({ack,NodeId,MsgId}),HostList),
       loop(Socket,HostList);
	{ack, NodeId, MsgId} ->
       AckRecv = {ack,NodeId,MsgId},
       AckList -- AckRecv,

       case map(_,AckList) of
        true -> process message?
        false -> loop(Socket,HostList);
%% On receive {ack, NodeId, Msgid}:
%% 	   remove ack from acklist
%% 	   if acklist is empty,
%% 	       process the message

%%        loop(Socket, HostList);
	{add, HostConfig} ->
	    io:format("Adding ~p to HostList ~p~n", [HostConfig, HostList]),
	    {Id, Host, Port} = HostConfig,
	    HostConfig1 = {Id, hostname_to_ip(Host), Port},
	    loop(Socket, [HostConfig1 | (HostList -- [HostConfig1])]);
	{remove, Id} ->
	    {ok, HostConfig} = find_host(Id, HostList),
	    io:format("Removing ~p from HostList ~p~n", [HostConfig, HostList]),
	    loop(Socket, (HostList -- [HostConfig]));
	{become, NewLoop} ->
	    io:format("Running a new loop function ~p~n", [NewLoop]),
	    NewLoop(Socket, HostList)
    end.

%% Sender: {multicast, Msg} to messagepasser

%% MessagePasser: 
%% bsend({rmulti, MsgId, Msg})

%% On receive {rmulti, MsgId, Msg}:
%% 	   Create Acklist [{ack, NodeId_1, MsgId}, ... ]
%% 	   bsend({ack, MyNode, Msgid})

%% On receive {ack, NodeId, Msgid}:
%% 	   remove ack from acklist
%% 	   if acklist is empty,
%% 	       process the message

