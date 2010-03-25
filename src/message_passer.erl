-module(message_passer).
-compile([export_all]).

-import(queue, [out/1, in/2]).

% message passer does two things
% 1. sends messages over tcp/udp to specific host/multicasts to all 
% 2. receives a messages of tcp/udp and then route the message to game_logic or game_manager
% 3. locks

% the game_logic and game_manager processes are registered processes

-record(server_state, {host_list, msgid = 0, acklist = [], myid, hold_queue = [], lock_state, timestamp = 0}).

start(Port, HostList, MyId) ->
    spawn(fun() -> server(Port, HostList, MyId) end),
    ok.

stop() ->
	message_passer ! {die}.

server(Port, HostList, MyId) ->
     {ok, Socket} = gen_udp:open(Port, [binary]),
     register(message_passer, self()),
     HostList_tmp = [{MyId, "localhost", Port} | HostList],
     HostList1 = [{Id, hostname_to_ip(Host), Port1} || {Id, Host, Port1} <- HostList_tmp],
     ServerState = #server_state{host_list=HostList1, myid=MyId},
     loop(Socket, ServerState).

usend(Socket, Id, Msg, HostList) ->
    {ok, {Id, Host, Port}} = find_host(Id, HostList),
    ok = gen_udp:send(Socket, Host, Port, term_to_binary(Msg)).

hostname_to_ip(HostName) ->
    {ok, Ip} = inet:getaddr(HostName, inet),
    Ip.
    
bsend(Socket, Msg, HostList) ->
    lists:foreach(fun ({_, Host, Port}) -> gen_udp:send(Socket, Host, Port, term_to_binary(Msg)) end, HostList).

find_host(Id, [{Id, Host, Port} | _]) -> {ok, {Id, Host, Port}};
find_host(Id, [ _ | HostList]) -> find_host(Id, HostList);
find_host(_, []) -> not_found.

find_id({Host, Port}, [{Id, Host, Port} | _HostList]) -> {ok, {Id, Host, Port}};
find_id(Key, [ _ | HostList]) -> find_id(Key, HostList);
find_id(_, []) -> not_found.

find_source_message({Source,MsgId},[{_,_,Source,MsgId,_}| _AckList]) -> found;
find_source_message(Key, [_ | AckList]) -> find_source_message(Key,AckList);
find_source_message(_,[]) -> not_found.

delete_source_message({Source,MsgId},{_,_,MyId,MesgID,_}) -> found;
delete_source_message(_,_) -> not_found.

%TODO: implement this!!
check_lock_replies({},[{_Type, NodeId,HostId,MsgId,_} | _LockReply]) ->
	nothing.
	

%% this is primarily called when a message needs to be sent to other processes like
%% game_logic
route_message(Msg, HostId) ->
    io:format("Route Message ~p from ~p~n", [Msg, HostId]),
    case Msg of
 	{game_logic, Body} ->
	    io:format("matched game_logic so forwarding ~p~n", [Body]),
 	    game_logic ! Body;
 	{game_manager, Body} ->
 	    game_manager ! Body;
	_ ->
	    message_passer ! Msg
    end.
    
route_message(Msg, Host, Port, HostList) ->
    {ok , {Id, Host, Port}} = find_id({Host, Port}, HostList),
    route_message(Msg, Id).

get_lock(onResource) ->
	message_passer ! {getLock, onResource}.    
	
%send a broadcast message
broadcast(Msg) ->
    message_passer ! {broadcast, Msg}.

compare({_Type, MyId1, _MsgId, _Msg, NwTimeStamp1} = McastMsg1, {_Type, MyId2, _MsgId, _Msg, NwTimeStamp2} = McastMsg2) ->
	case NwTimeStamp1 < NwTimeStamp2 of 
		true ->
			true;
		false ->
			case NwTimeStamp1 =:= NwTimeStamp2 of
				true ->
					MyId1 < MyId2;
				false ->
					false
			end
	end.

compare({MyId1, NwTimeStamp1}, {MyId2,NwTimeStamp2}) ->
	case NwTimeStamp1 < NwTimeStamp2 of 
		true ->
			true;
		false ->
			case NwTimeStamp1 =:= NwTimeStamp2 of
				true ->
					MyId1 < MyId2;
				false ->
					false
			end
	end.

loop(Socket, ServerState) ->
    HostList = ServerState#server_state.host_list,
    MsgId = ServerState#server_state.msgid,
	TimeStamp = ServerState#server_state.timestamp,
    receive
	{udp, Socket, Host, Port, BinMsg} ->
	    io:format("Received udp message from ~p : ~p~n", [{Host, Port}, BinMsg]),
	    route_message(binary_to_term(BinMsg), Host, Port, HostList),
	    loop(Socket, ServerState);
	{unicast, Id, Msg} ->
	    io:format("Sending unicast message to id ~p : ~p~n", [Id, Msg]),
	    usend(Socket, Id, Msg, HostList),
	    loop(Socket, ServerState);
	{broadcast, Msg} ->
	    io:format("Sending broadcast message : ~p~n", [Msg]),
	    bsend(Socket, Msg, HostList),
	    loop(Socket, ServerState);
	{multicast, Msg} ->
	    %% reliable multicast
	    io:format("Sending multicast message : ~p~n", [Msg]),
 	    NewMsgId = MsgId + 1,
 		NewTimeStamp = TimeStamp + 1,
 	    McastMsg = {rmulti,ServerState#server_state.myid,NewMsgId, Msg, NewTimeStamp},
 	    bsend(Socket, McastMsg, HostList),
	    loop(Socket, ServerState#server_state{msgid = NewMsgId, timestamp = NewTimeStamp});
	{rmulti, HostId, MId, Msg, TimeStamp} = McastMsg ->
	    %% On receive {rmulti, MsgId, Msg}:
	    %% 	   Create Acklist [{ack, NodeId_1, MsgId}, ... ]
	    %% 	   bsend({ack, MyNode, Msgid})
	    %% maybe exclude Me in acklist
	    %% 
		
		case Msg of ->
			{lockRequest,_} ->
				message_passer ! {lockRequest,HostId,MId,TimeStamp},
			{_,_} ->
				io:format("Normal Multicast : ~p~n",[Msg])
		end,

	    Me = ServerState#server_state.myid,
		MyTimeStamp = ServerState#server_state.time_stamp,
		
		NewTimeStamp1 = case TimeStamp > MyTimeStamp of
			true ->
					TimeStamp + 1;
			false ->
					MyTimeStamp + 1
		end,

	    AckList = [{ack,NodeId,HostId,MId,NewTimeStamp1} || {NodeId,_Ip,_Port} <- HostList],

	    %% send your acks
	    bsend(Socket,{ack,Me,HostId,MsgId,NewTimeStamp1},HostList),

	    %% add to hold queue
	    HQ = ServerState#server_state.hold_queue,
	    NewHQ = [McastMsg | HQ],
		
		%% sort the hold queue, based on the logical timestamps
		SortedHQ = lists:sort(fun compare/2, NewHQ),
	    
	    loop(Socket,ServerState#server_state{acklist=AckList, hold_queue = SortedHQ});
	
	{ack, _AckNode, Source, MsgId, _TimeStamp} = Ack ->
	    AckList = ServerState#server_state.acklist,
	    NewAckList = AckList -- Ack,
		HoldQueue = ServerState#server_state.hold_queue,
	    %% if newacklist is empty
	    %% BUGGGG: fix a bug here with getting the message from the head of the hold queue
	    case find_source_message(Source,MsgId, NewAckList) of
		found ->
		    loop(Socket, ServerState);
		not_found ->
			Head = [McastMsg3 | HoldQueue],
			NewHoldQueue = case delete_source_message(Source,MsgId,McastMsg3) of
								found ->
									HoldQueue,
									%% process the message
		    						route_message(Source, Msg)
								not_found ->
					 				Head
						   end,	
	    end,
	    loop(Socket, ServerState#server_state{acklist=NewAckList,hold_queue = NewHoldQueue});
	{getLock, _Something} ->
		LockMessage = {lockRequest, _},
		message_passer ! {multicast,LockMessage},
%%		bsend(Socket, LockMessage, HostList),
		loop(Socket, ServerState);
 	{lockRequest, HostId, MsgId, ReqTimeStamp} ->
 		LockState = ServerState#server_state.lock_state,
		MyTimeStamp1 = ServerState#server_state.timestamp,
		NodeId = ServerState#server_state.myid,
 		case LockState of
 			released -> 
 				ReplyMessage = {lockReply,NodeId,HostId,MsgId,'OK'},
				message_passer ! {multicast,ReplyMessage},
%% 				bsend(Socket, LockMessage, HostList),
 				loop(Socket,ServerState);
 			held ->
 				loop(Socket,ServerState);
 			wanted ->
 				% compare timestamps
				case compare({HostId,ReqTimeStamp},{NodeId,MyTimeStamp1}) of
					true ->
						ReplyMessage = {lockReply,NodeId,HostId,MsgId,'OK'},
						message_passer ! {multicast,ReplyMessage},
					false ->
						loop(Socket,ServerState),
				end,

 				loop(Socket,ServerState)
 		end;
			
 	{lockReply, NodeId, HostId, MsgId, 'OK'} = LReply1 ->
		%% for each node in the hostlist, check if OK reply received,
		%% if true, takelock(),else loop back
			LockReply = [LReply1 | _],
			case check_lock_replies(LockReply) of
				found ->
					loop(Socket,ServerState),					
				not_found ->
					%TODO: implement take_lock
					take_lock(OnResource),
			end,
		loop(Socket,ServerState);
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
	{die} ->
		io:format("message_passer closing...~n");
	Any ->
	    io:format("Ignoring unmatched message ~p~n", [Any]),
		loop(Socket, ServerState)
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
% bcast lockrequest ?? when to initiate this lock request??
% wait for all lockreply
% acquired the lock so return

%% on receive lockrequest()
% check your state {released, held, wanted}
% reply when you can


% a list of locks (atoms)
% how to this without blocking message passer

% how to tell mp to acquire a lock
% how to know when lock is acquired
