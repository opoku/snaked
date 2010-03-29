-module(message_passer).
-compile([export_all]).

-import(queue, [out/1, in/2]).

%% message passer does two things
%% 1. sends messages over tcp/udp to specific host/multicasts to all 
%% 2. receives a messages of tcp/udp and then route the message to game_logic or game_manager
%% 3. locks

%% the game_logic and game_manager processes are registered processes

-record(server_state, {host_list, msgid = 0, acklist = [], myid, hold_queue = [], lock_state_list, timestamp = 0}).

start(Port, HostList, MyId) ->
    spawn(fun() -> server(Port, HostList, MyId) end),
    ok.

stop() ->
    message_passer ! {die}.

debug() ->
    message_passer ! {debug, self()},
    receive
	{message_passer, Any} ->
	    Any
    end.

ping_test(Count, TimeOut) ->
    register(pong_listener, self()),
    ping_test(0, Count, TimeOut).

ping_test(Count, Count, TimeOut) ->
    RecvList = receive_pongs(TimeOut, []),
    Sum = lists:sum([I || {_, I} <- RecvList]),
    io:format("Number of Messages Received ~p~n", [Sum]);
	    
ping_test(I, Count, Timeout) ->
    case Count rem 10 of
	0 ->
	    sleep(50);
	_ ->
	    nothing
    end,
    message_passer ! {broadcastping, I},
    ping_test(I+1, Count, Timeout).

sleep(Time) ->
    receive
    after
	Time ->
	    done
	      
    end.

receive_pongs(TimeOut, SeqCountList) ->
    receive
	{pong, _Id, Seq} ->
	    NewList = case lists:keyfind(Seq, 1, SeqCountList) of
			  {Seq, Count} ->
			      lists:keystore(Seq, 1, SeqCountList, {Seq, Count+1});
			  false ->
			      lists:keystore(Seq, 1, SeqCountList, {Seq, 1})
		      end,
	    receive_pongs(TimeOut, NewList)
    after
	TimeOut ->
	    lists:keysort(1, SeqCountList)
    end.

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

get_lock(OnResource) ->
    message_passer ! {getLock, self(), OnResource},
    receive
	{acquired, message_passer, OnResource} ->
	    acquired;
	{failed, message_passer, OnResource, Msg} ->
	    {failed, Msg}
    end.

release_lock(OnResource) ->
    message_passer ! {releaseLock, self(), OnResource},
    receive
	{released, message_passer, OnResource} ->
	    released;
	{failed, message_passer, OnResource, Msg} ->
	    {failed, Msg}
    end.

						%send a broadcast message
broadcast(Msg) ->
    message_passer ! {broadcast, Msg}.

compare({rmulti, MyId1, _MsgId1, _Msg1, NwTimeStamp1}, {rmulti, MyId2, _MsgId2, _Msg2, NwTimeStamp2}) ->
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
    end;

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
    receive
	{udp, Socket, Host, Port, BinMsg} ->
	    HostList = ServerState#server_state.host_list,
	    MsgTerm = binary_to_term(BinMsg),
	    io:format("Received udp message from ~p : ~p~n", [{Host, Port}, MsgTerm]),
	    route_message(MsgTerm, Host, Port, HostList),
	    loop(Socket, ServerState);
	{unicast, Id, Msg} ->
	    HostList = ServerState#server_state.host_list,
	    io:format("Sending unicast message to id ~p : ~p~n", [Id, Msg]),
	    usend(Socket, Id, Msg, HostList),
	    loop(Socket, ServerState);
	{broadcast, Msg} ->
	    HostList = ServerState#server_state.host_list,
	    io:format("Sending broadcast message : ~p~n", [Msg]),
	    bsend(Socket, Msg, HostList),
	    loop(Socket, ServerState);
	{multicast, Msg} ->
	    %% reliable multicast
	    io:format("Sending multicast message : ~p~n", [Msg]),
	    HostList = ServerState#server_state.host_list,
	    TimeStamp = ServerState#server_state.timestamp,
	    MsgId = ServerState#server_state.msgid,
 	    NewMsgId = MsgId + 1,
	    NewTimeStamp = TimeStamp + 1,
 	    McastMsg = {rmulti,ServerState#server_state.myid,NewMsgId, Msg, NewTimeStamp},
 	    bsend(Socket, McastMsg, HostList),
	    loop(Socket, ServerState#server_state{msgid = NewMsgId, timestamp = NewTimeStamp});
	{rmulti, HostId, MId, _Msg, MsgTimeStamp} = McastMsg ->
	    %% On receive {rmulti, MsgId, Msg}:
	    %% 	   Create Acklist [{ack, NodeId_1, MsgId}, ... ]
	    %% 	   bsend({ack, MyNode, Msgid})
	    %% maybe exclude Me in acklist
	    %% 
	    io:format("Processing Multicast message from ~p ~n", [McastMsg]),
	    #server_state{myid=Me, timestamp=MyTimeStamp, acklist=PrevAckList, host_list = HostList} = ServerState,

	    %% updating my own timestamp
	    NewTimeStamp1 = case MsgTimeStamp > MyTimeStamp of
				true ->
				    MsgTimeStamp + 1;
				false ->
				    MyTimeStamp + 1
			    end,

	    AckList = [{ack,NodeId,HostId,MId,NewTimeStamp1} || {NodeId,_Ip,_Port} <- HostList],
	    AllAckList = AckList ++ PrevAckList,
	    %% send your acks
	    message_passer ! {broadcast, {ack,Me,HostId,MId,NewTimeStamp1}},

	    %% add to hold queue
	    HQ = ServerState#server_state.hold_queue,
	    NewHQ = [McastMsg | HQ],

	    %% sort the hold queue, based on the logical timestamps
	    SortedHQ = lists:sort(fun compare/2, NewHQ),

	    io:format("Contents of Hold queue ~p~n", [SortedHQ]),

	    loop(Socket,ServerState#server_state{acklist=AllAckList, hold_queue = SortedHQ});
	{ack, _AckNode, Source, MsgId, _TimeStamp} = Ack ->
	    io:format("Received ack ~p~n", [Ack]),
	    AckList = ServerState#server_state.acklist,
	    NewAckList = AckList -- [Ack],
	    HoldQueue = ServerState#server_state.hold_queue,
	    %% if newacklist is empty
	    %% BUGGGG: fix a bug here with getting the message from the head of the hold queue
	    case find_source_message({Source,MsgId}, NewAckList) of
		found ->
		    loop(Socket, ServerState#server_state{acklist = NewAckList});
		not_found ->
		    [{rmulti, HostId, MId, Msg, MsgTimeStamp} | Rest] = HoldQueue,
		    %%		    Head = [McastMsg3 | HoldQueue],
		    %% check if source and msgid field in mcastmsg3 is equal to Source, MsgId variables
		    NewHoldQueue = case (MId =:= MsgId) and (Source =:= HostId) of
				       true ->
					   %% process the message
					   case Msg of 
					       {lockRequest,ResourceId} ->
						   message_passer ! {lockRequest,HostId,MsgTimeStamp,ResourceId};
					       Any ->
						   io:format("Normal Multicast : ~p~n",[Any]),
						   route_message(Source, Any)
					   end,
					   Rest;
				       false ->
					   HoldQueue
				   end,
		    loop(Socket, ServerState#server_state{acklist=NewAckList,hold_queue = NewHoldQueue})
		end;
	{getLock, Pid, ResourceId} ->
	    io:format("CAlling Getlock for ~p~n", [ResourceId]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    MyNodeId = ServerState#server_state.myid,
	    HostList = ServerState#server_state.host_list,
	    case lists:key_find(ResourceId, 1, LockStateList) of
		false ->
		    ReplyList = [{lockReply, Node, MyNodeId} || {Node, _, _} <- HostList],
		    NewLockStateList = lists:key_store(ResourceId, 1, LockStateList,
						       {ResourceId, released, queue:new(), ReplyList, Pid}),
		    LockMessage = {lockRequest, ResourceId},
		    message_passer ! {multicast,LockMessage},
		    loop(Socket, ServerState#server_state{lock_state_list = NewLockStateList});
		_Any ->
		    %% the reply list is not empty
		    %% reply with failure
		    Pid ! {failed, message_passer, ResourceId, {error, duplicate_request, ResourceId}},
		    loop(Socket, ServerState)
	    end;
	{releaseLock, Pid, ResourceId} ->
	    io:format("CAlling releaseLock for ~p~n", [ResourceId]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    MyNodeId = ServerState#server_state.myid,
	    case lists:key_find(ResourceId, 1, LockStateList) of
		{ResourceId, held, RequestQueue, [], Pid} ->
		    lists:foreach(fun (RequestMsg) ->
					  {lockRequest, HostId, _ReqTimeStamp, ResourceId} = RequestMsg,
					  LockReply = {lockReply, MyNodeId, HostId, ResourceId},
					  message_passer ! {unicast, HostId, LockReply}
				  end,
				  queue:to_list(RequestQueue)),
		    NewLockStateList = lists:key_delete(ResourceId, 1, LockStateList),
		    Pid ! {released, message_passer, ResourceId},
		    loop(Socket, ServerState#server_state{lock_state_list = NewLockStateList});
		false ->
		    Pid ! {failed, message_passer, {error, lock_not_held, ResourceId}},
		    loop(Socket, ServerState)
	    end;
 	{lockRequest, HostId, ReqTimeStamp, ResourceId} = RequestMsg ->
	    %%	    {resourceid, lock_state}
	    io:format("Received lockRequest ~p~n", [RequestMsg]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    {ResourceId, LockState, RequestQueue, ReplyList, Pid} =
		case lists:key_find(ResourceId, 1, LockStateList) of
		    {ResourceId, _, _, _, _} = Item ->
			Item;
		    false ->
			{ResourceId, released, queue:new(), [], none}
		end,
	    MyNodeId = ServerState#server_state.myid,
	    case {HostId, LockState} of
		{MyNodeId, held} ->
		    %% this is me.. should be there
		    loop(Socket, ServerState);
		{MyNodeId, released} ->
		    %% this is me so change to wanted
		    NewLockStateList = lists:key_store(ResourceId, 1, LockStateList,
						       {ResourceId, {wanted, ReqTimeStamp}, RequestQueue, ReplyList, Pid}),
		    ReplyMessage = {lockReply,MyNodeId,HostId,ResourceId},
		    message_passer ! {unicast,HostId,ReplyMessage},
		    loop(Socket, ServerState#server_state{lock_state_list = NewLockStateList});
		{_, released} ->
		    %% reply immediately
		    ReplyMessage = {lockReply,MyNodeId,HostId,ResourceId},
		    message_passer ! {unicast,HostId,ReplyMessage},
		    loop(Socket,ServerState);
		{_, held} ->
		    %% add the request to my request queue
		    NewRequestQueue = queue:in(RequestMsg, RequestQueue),
		    NewLockStateList = lists:key_store(ResourceId, 1, LockStateList,
						       {ResourceId, LockState, NewRequestQueue, ReplyList, Pid}),
		    loop(Socket,ServerState#server_state{lock_state_list=NewLockStateList});
		{_, {wanted, MyReqTimeStamp}} ->
		    %% compare timestamps
		    case compare({HostId,ReqTimeStamp},{MyNodeId,MyReqTimeStamp}) of
			true ->
			    %% a < b
			    ReplyMessage = {lockReply,MyNodeId,HostId,ResourceId},
			    message_passer ! {unicast, HostId ,ReplyMessage},
			    loop(Socket, ServerState);
			false ->
			    %% a > b
			    NewRequestQueue = queue:in(RequestMsg, RequestQueue),
			    NewLockStateList = lists:key_store(ResourceId, 1, LockStateList,
							       {ResourceId, LockState, NewRequestQueue, ReplyList, Pid}),
			    loop(Socket,ServerState#server_state{lock_state_list=NewLockStateList})
		    end
	    end;

 	{lockReply, NodeId, HostId, ResourceId} ->
	    %% for each node in the hostlist, check if OK reply received,
	    %% if true, takelock(),else loop back
	    io:format("Received a lockReply from ~p~n", [NodeId]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    {ResourceId, LockState, RequestQueue, ReplyList, ReqPid} = lists:key_find(ResourceId, 1, LockStateList),
	    LReply2 = {lockReply, NodeId, HostId},
	    NewReplyList = ReplyList -- LReply2,

	    %%	    case check_lock_replies(LockReply) of
	    case NewReplyList of
		[] ->
		    NewLockStateList = lists:key_store(ResourceId, 1, LockStateList, {ResourceId, held, queue:new(), []}),
		    %%  TODO: tell requestor of lock that we have acquired lock
		    ReqPid ! {acquired, message_passer, ResourceId},
		    loop(Socket, ServerState#server_state{lock_state_list = NewLockStateList});
		_NotEmpty ->
		    NewLockStateList = lists:key_store(ResourceId, 1, LockStateList,
						       {ResourceId, LockState, RequestQueue, NewReplyList, ReqPid}),
		    loop(Socket, ServerState#server_state{lock_state_list = NewLockStateList})
	    end;
	{add, HostConfig} ->
	    HostList = ServerState#server_state.host_list,
	    io:format("Adding ~p to HostList ~p~n", [HostConfig, HostList]),
	    {Id, Host, Port} = HostConfig,
	    HostConfig1 = {Id, hostname_to_ip(Host), Port},
	    loop(Socket, ServerState#server_state{host_list=[HostConfig1 | (HostList -- [HostConfig1])]});
	{remove, Id} ->
	    HostList = ServerState#server_state.host_list,
	    {ok, HostConfig} = find_host(Id, HostList),
	    io:format("Removing ~p from HostList ~p~n", [HostConfig, HostList]),
	    loop(Socket, ServerState#server_state{host_list=(HostList -- [HostConfig])});
	{become, NewLoop} ->
	    io:format("Running a new loop function ~p~n", [NewLoop]),
	    NewLoop(Socket, ServerState);
	{die} ->
	    io:format("message_passer closing...~n");
	{debug, Pid} ->
	    Pid ! {message_passer, ServerState},
	    loop(Socket, ServerState);
	{broadcastping, Seq} ->
	    MyId = ServerState#server_state.myid,
	    message_passer ! {broadcast, {ping, MyId, Seq}},
	    loop(Socket, ServerState);
	{ping, HostId, Seq} ->
	    MyId = ServerState#server_state.myid,
	    message_passer ! {unicast, HostId, {pong,MyId,Seq}},
	    loop(Socket, ServerState);
	{pong, _, _} = Pong ->
	    pong_listener ! Pong,
	    loop(Socket, ServerState);
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
