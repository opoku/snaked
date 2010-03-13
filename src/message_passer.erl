-module(message_passer).
-compile([export_all]).

-import(queue, [out/1, in/2]).

% message passer does two things
% 1. sends messages over tcp/udp to specific host/multicasts to all 
% 2. receives a messages of tcp/udp and then route the message to game_logic or game_manager
% 3. locks

% the game_logic and game_manager processes are registered processes

-record(server_state, {host_list, msgid = 0, acklist = [], myid, hold_queue=queue:new()}).

start(Port, HostList, MyId) ->
    spawn(fun() -> server(Port, HostList, MyId) end).

server(Port, HostList, MyId) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    register(message_passer, self()),
    HostList1 = [{Id, hostname_to_ip(Host), Port1} || {Id, Host, Port1} <- HostList],
    ServerState = #server_state{host_list=HostList1, myid=MyId},
    loop(Socket, ServerState).

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


%% this is primarily called when a message needs to be sent to other processes like
%% game_logic
route_message(Msg, HostId) ->
    io:format("Route Message ~p from ~p~n", [Msg, HostId]),
    case element(1, Msg) of
%% 	game_logic ->
%% 	    game_logic ! Msg;
%% 	game_manager ->
%% 	    game_manager ! Msg;
	_ ->
	    message_passer ! Msg
    end.
    
route_message(Msg, Host, Port, HostList) ->
    {ok , {Id, Host, Port}} = find_id({Host, Port}, HostList),
    route_message(Msg, Id).

loop(Socket, ServerState) ->
    receive
	{udp, Socket, Host, Port, BinMsg} ->
	    io:format("Received udp message from ~p : ~p~n", [{Host, Port}, Msg]),
	    route_message(binary_to_term(BinMsg), Host, Port, HostList),
	    loop(Socket, ServerState);
	{unicast, Id, Msg} ->
	    io:format("Sending unicast message to id ~p : ~p~n", [Id, Msg]),
	    usend(Socket, Id, term_to_binary(Msg), HostList),
	    loop(Socket, ServerState);
	{broadcast, Msg} ->
	    io:format("Sending broadcast message : ~p~n", [Msg]),
	    bsend(Socket, term_to_binary(Msg), HostList),
	    loop(Socket, ServerState);
	{multicast, Msg} ->
	    %% reliable multicast
	    io:format("Sending multicast message : ~p~n", [Msg]),
	    NewMsgId = MsgId + 1,
	    McastMsg = {rmulti, ServerState#server_state.myid, NewMsgId, Msg},
	    bsend(Socket, term_to_binary(McastMsg), HostList),
	    loop(Socket, ServerState#server_state{msgid=NewMsgId});
	{rmulti, HostId, MId, Msg} ->
	    %% On receive {rmulti, MsgId, Msg}:
	    %% 	   Create Acklist [{ack, NodeId_1, MsgId}, ... ]
	    %% 	   bsend({ack, MyNode, Msgid})

	    %% maybe exclude Me in acklist
	    %% 
	    Me = ServerState#server_state.myid,
	    AckList = [{ack,NodeId,HostId,MId} || {NodeId,Ip,Port} -> HostList ],

	    %% send your acks
	    bsend(Socket,term_to_binary({ack,Me,HostId,MsgId}),HostList),

	    %% add to hold queue
	    HQ = ServerState#server_state.hold_queue,
	    NewHQ = in(McastMsg, HQ),
	    
	    loop(Socket,ServerState#server_state{acklist=AckList, hold_queue = NewHQ});
	
	{ack, AckNode, Source, MsgId} = Ack ->
	    AckList = ServerState#server_state.acklist,
	    NewAckList = AckList -- Ack,
	    %% if newacklist is empty
	    case find_source_message(Source,MsgId, NewAckList) of
		found ->
		    loop(Socket, ServerState);
		notfound ->
		    %% process the message
		    route_message(Source, Msg)
	    end,
	    loop(Socket, ServerState#server_state{acklist=NewAckList});

	{lockRequest, SomeThing} ->
	    nothing;

	{add, HostConfig} ->
	    io:format("Adding ~p to HostList ~p~n", [HostConfig, HostList]),
	    {Id, Host, Port} = HostConfig,
	    HostConfig1 = {Id, hostname_to_ip(Host), Port},
	    loop(Socket, ServerState#server_state{host_list=[HostConfig1 | (HostList -- [HostConfig1])]});
	{remove, Id} ->
	    {ok, HostConfig} = find_host(Id, HostList),
	    io:format("Removing ~p from HostList ~p~n", [HostConfig, HostList]),
	    loop(Socket, ServerState#server_state{host_list=(HostList -- [HostConfig])});
	{become, NewLoop} ->
	    io:format("Running a new loop function ~p~n", [NewLoop]),
	    NewLoop(Socket, ServerState);
	Any ->
	    io:format("Ignoring unmatched message ~p~n", [Any])
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



%% lock() 
% bcast lockrequest
% wait for all lockreply
% acquired the lock so return

%% on receive lockrequest()
% check your state {released, held, wanted}
% reply when you can


% a list of locks (atoms)
% how to this without blocking message passer

% how to tell mp to acquire a lock
% how to know when lock is acquired
