-module(message_passer).
-compile([export_all]).

% message passer does two things
% 1. sends messages over tcp/udp to specific host/multicasts to all 
% 2. receives a messages of tcp/udp and then route the message to game_logic or game_manager

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

route_message(Host, Port, Msg, HostList) ->
    {ok , {Id, Host, Port}} = find_id({Host, Port}, HostList),
    io:format("Route Message ~p from ~p~n", [Msg, Id]).

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
	{multicast, Msg} ->
	    io:format("Sending multicast message : ~p~n", [Msg]),
	    bsend(Socket, term_to_binary(Msg), HostList),
	    loop(Socket, HostList);
	{add, HostConfig} ->
	    io:format("Adding ~p to HostList ~p~n", [HostConfig, HostList]),
	    {Id, Host, Port} = HostConfig,
	    HostConfig1 = {Id, hostname_to_ip(Host), Port},
	    loop(Socket, [HostConfig1 | (HostList -- [HostConfig1])]);
	{remove, Id} ->
	    {ok, HostConfig} = find_host(Id, HostList),
	    io:format("Removing ~p from HostList ~p~n", [HostConfig, HostList]),
	    loop(Socket, (HostList -- [HostConfig]))
    end.
