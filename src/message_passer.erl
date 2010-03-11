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
    {ok, Socket} = gen_udp:open(Port, [list]),
    loop(Socket, HostList).

usend(Socket, Id, Msg, HostList) ->
    {ok, Host, Port} = find_host(Id, HostList),
    ok = gen_udp:send(Socket, Host, Port, Msg).
    
bsend(Socket, Msg, HostList) ->
    lists:foreach(fun ({Id, Host, Port}) -> gen_udp:send(Socket, Host, Port, Msg) end, HostList).

loop(Socket, HostList) ->
    receive
	{udp, Socket, Host, Port, Msg} ->
	    route_message(Host, Msg, HostList),
	    loop(Socket, HostList);
	{unicast, Id, Msg} ->
	    usend(Socket, Id, Msg, HostList),
	    loop(Socket, HostList);
	{multicast, Msg} ->
	    bsend(Socket, Msg, HostList),
	    loop(Socket, HostList);
	{add, HostConfig} ->
	    loop(Socket, [HostConfig | (HostList -- [HostConfig])]);
	{remove, HostConfig} ->
	    loop(Socket, (HostList -- [HostConfig]))
    end.
