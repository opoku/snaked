-module(message_passer).
-compile([export_all]).

-import(queue, [out/1, in/2]).

%% message passer does two things
%% 1. sends messages over tcp/udp to specific host/multicasts to all 
%% 2. receives a messages of tcp/udp and then route the message to game_logic or game_manager
%% 3. locks

%% the game_logic and game_manager processes are registered processes

%% registered list is a tuple of {NodeId, Pid, Host, Port}.  The Pid is the tcp_comm
%% process that is responsible for the open socket that sends messages to the NodeId

%% adding another field called status which is either guest or player

-define(COM_MODULE,tcp_comm). %% can alternately be udp_comm

-record(host_info, {nodeid, pid, host, port,
		    %% status is either guest or player
		    status = guest}).

-record(server_state, {registered_list = [],
		       msgid = 0, acklist = [], myid,
		       myport,
		       hold_queue = [],
		       lock_state_list = [],
		       timestamp = 0,
		       pid_list = [],

		       %% keeps track of the last message id received from each host
		       msg_tracker = [],
                       
               %% a list of processes waiting on connect() call to receive the nodeId
               connect_wait_queue = []
		      }).

start(Port, MyId) ->
    start(Port, MyId, []).

start(Port, MyId, HostList) ->
    Pid = spawn_link(message_passer, server, [Port, MyId]),
    register(message_passer, Pid),
    spawn(fun() ->
		  message_passer ! {listen, Port},
		  lists:foreach(fun ({Host, HostPort}) -> connect(Host, HostPort) end, [{{127,0,0,1}, Port}| HostList]),
		  io:format("Finished connecting~n")
	  end),
    Pid.
    
stop() ->
    kill_comm_processes(),
    message_passer ! {die}.

server(Port, MyId) ->
    %% don't die when you receive exit messages from your comms
    process_flag(trap_exit, true),
    loop(#server_state{myid = MyId, myport = Port}).

broadcast(Msg) ->
    message_passer ! {broadcast, Msg},
    ok.

multicast(Msg) ->
    message_passer ! {multicast, Msg},
    ok.

unicast(NodeId, Msg) ->
    message_passer ! {unicast, NodeId, Msg},
    ok.

connect(Host, Port) ->
    message_passer ! {connect, self(), Host, Port},
    receive
       {message_passer, {NodeId, {_HostIP, _HostPort}}} ->
            NodeId;
       {message_passer, error} ->
            {error}
    end.

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

make_player(NodeId) ->
    message_passer ! {make_player, NodeId}.

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



%% this is primarily called when a message needs to be sent to other processes like
%% game_logic
route_message(Msg) ->
    %%io:format("Route message ~p~n", [Msg]),
    case Msg of
	{game_logic, Body} ->
	    game_logic ! Body;
	{game_manager, Body} ->
	    game_manager ! Body;
	_ ->
	    message_passer ! Msg
    end.
    

route_message(_HostId, Msg) ->
    %%io:format("Route Message ~p from ~p~n", [Msg, HostId]),
    route_message(Msg).

find_source_message({Source,MsgId},[{_,_,Source,MsgId}| _AckList]) -> found;
find_source_message(Key, [_ | AckList]) -> find_source_message(Key,AckList);
find_source_message(_,[]) -> not_found.

is_player(#host_info{status=player}) ->
    true;
is_player(#host_info{status=guest}) ->
    false.

is_guest(#host_info{status=player}) ->
    false;
is_guest(#host_info{status=guest}) ->
    true.

usend(Id, Msg, RegisteredList) ->
    %%io:format("Sending unicast message to id ~p : ~p~n", [Id, Msg]),
    #host_info{pid=Pid} = lists:keyfind(Id, #host_info.nodeid, RegisteredList),
    ok = tcp_comm:send_msg(Pid, Msg).

bsend(Msg, RegisteredList) ->
    %%io:format("Sending broadcast message : ~p~n", [Msg]),
    lists:foreach(fun (#host_info{pid=Pid}) -> ok = tcp_comm:send_msg(Pid, Msg)
		  end, RegisteredList).

loop(ServerState) ->
    receive
	%% communication with processes
	{recvdata, Pid, Data} ->
	    %% recv'd data from external node.. lets decide what do with it
	    %%io:format("------Received message ~p~n", [Data]),
	    case Data of 
		{register, NodeId, Port} ->
		    Ip = tcp_comm:get_host_ip(Pid),
		    io:format("Registering nodeid ~p:~p~p~n", [NodeId, Ip, Port]),
		    #server_state{registered_list = RegisteredList, msg_tracker = MessageTrackingList, connect_wait_queue = ConnectWaitQueue} = ServerState,
		    NewRegisteredList = case lists:keyfind(NodeId, #host_info.nodeid, RegisteredList) of
					    #host_info{nodeid=NodeId} ->
						%% node id already exists so tell new connection to select a new id
						io:format("Nodeid ~p already exists~n", [NodeId]),
						%%ok = tcp_comm:send_msg(Pid, {error, nodeid_exists, NodeId}),
						case lists:keyfind({Ip, Port}, 2, ConnectWaitQueue) of
						    {Pid2, {HostIP, HostPort}} ->
							Pid2 ! {message_passer, {NodeId, {HostIP, HostPort}}};
						    false ->
							done
							%%Pid2 ! {message_passer, error}
						end,
						RegisteredList;
					    false ->
						%% add node to registered list
						case lists:keyfind({Ip, Port}, 2, ConnectWaitQueue) of
						    {Pid3, {HostIP, HostPort}} ->
							Pid3 ! {message_passer, {NodeId, {HostIP, HostPort}}};
						    false ->
							done
							%%Pid3 ! {message_passer, error}
						end,
						lists:keystore(NodeId, #host_info.nodeid, RegisteredList,#host_info{nodeid=NodeId, pid=Pid, host=Ip, port=Port})
					end,
		    NewMessageTrackingList = lists:keystore(NodeId, 1, MessageTrackingList, {NodeId, -1}),
		    loop(ServerState#server_state{registered_list = NewRegisteredList, msg_tracker = NewMessageTrackingList});
		_Any ->
		    %%io:format("recvdata ~p~n",[Data]),
		    case lists:keyfind(Pid, #host_info.pid, ServerState#server_state.registered_list) of
			false ->
			    %% should not happend
			    io:format("I have been sent a network message from a process ~p I dont recognize~n", [Pid]);
			#host_info{status=player} ->
			    %% only route all messages from players
			    route_message(Data);
			#host_info{status=guest, nodeid=NodeId} ->
			    %% only route two types of messages from guests
			    case Data of
				{game_manager, {hello, NodeId}} ->
				    route_message(Data);
				{game_manager, {join, NodeId}} ->
				    route_message(Data);
				{game_manager, {ok, NodeId}} ->
				    route_message(Data);
				_Any ->
				    io:format("Dropping messsage ~p from guest ~p~n", [Data, NodeId])
			    end
		    end,
		    loop(ServerState)
	    end;
	{comm_started, Pid} ->
	    link(Pid),
	    PidList = ServerState#server_state.pid_list,
	    loop(ServerState#server_state{pid_list = [Pid | PidList]});
	{'EXIT', Pid, Reason} ->
	    RegisteredList = ServerState#server_state.registered_list,
	    case lists:keytake(Pid, #host_info.pid, RegisteredList) of
		{value, #host_info{nodeid=NodeId, pid=Pid, host=Host, port=Port}, NewRegisteredList} ->
		    case Reason of
			socketclosed ->
			    io:format("Socket for ~p{~p:~p} closed~n", [NodeId, Host, Port]);
			_Any ->
			    io:format("Some other reason for exiting (~p)~n", [Reason]),
			    nothing
		    end,
		    loop(ServerState#server_state{registered_list = NewRegisteredList});
		false ->
		    loop(ServerState)
	    end;
	{kill_comm} ->
	    PidList = ServerState#server_state.pid_list,
	    NewPidList = lists:filter(fun (Pid) -> catch (exit(Pid, kill)),
						   is_process_alive(Pid)
				      end, PidList),
	    loop(ServerState#server_state{pid_list = NewPidList});
	{listen, Port} ->
	    io:format("Start Server~n"),
	    tcp_comm:start_server(Port),
	    loop(ServerState);
	{connect, ConnectingNodeId, Host, Port} ->
	    io:format("Request for connect from~p~n", [ConnectingNodeId]),
	    ConnectWaitQueue = ServerState#server_state.connect_wait_queue,
	    NewConnectWaitQueue = [{ConnectingNodeId, {Host, Port}}|ConnectWaitQueue],
	    tcp_comm:start_client(Host,Port),
	    loop(ServerState#server_state{connect_wait_queue=NewConnectWaitQueue});
	{getinfo, Pid} ->
	    #server_state{myid = Id, myport = Port} = ServerState,
	    Pid ! {message_passer, {Id, Port}},
	    loop(ServerState);

	%% basic sending primitives
	{unicast, Id, Msg} ->
	    RegisteredList = ServerState#server_state.registered_list,
	    usend(Id, Msg, RegisteredList),
	    loop(ServerState);
	{broadcast, Msg} ->
	    %% only send messages to players
	    RegisteredList = lists:filter(fun is_player/1, ServerState#server_state.registered_list),
	    bsend(Msg, RegisteredList),
	    loop(ServerState);

	{broadcast_all, Msg} ->
	    %% send messages to everyone
	    RegisteredList = ServerState#server_state.registered_list,
	    bsend(Msg, RegisteredList),
	    loop(ServerState);

	%% multicast patterns
	{multicast, Msg} ->
	    %% reliable multicast
	    io:format("Sending multicast message : ~p~n", [Msg]),
	    RegisteredList = lists:filter(fun is_player/1, ServerState#server_state.registered_list),
	    TimeStamp = ServerState#server_state.timestamp,
	    MsgId = ServerState#server_state.msgid,
 	    NewMsgId = MsgId + 1,
	    NewTimeStamp = TimeStamp + 1,
 	    McastMsg = {rmulti,ServerState#server_state.myid,NewMsgId, Msg, NewTimeStamp},
 	    bsend(McastMsg, RegisteredList),
	    loop(ServerState#server_state{msgid = NewMsgId, timestamp = NewTimeStamp});
	{rmulti, HostId, MId, _Msg, MsgTimeStamp} = McastMsg ->
	    %% On receive {rmulti, MsgId, Msg}:
	    %% 	   Create Acklist [{ack, NodeId_1, MsgId}, ... ]
	    %% 	   bsend({ack, MyNode, Msgid})
	    %% maybe exclude Me in acklist
	    %% 
	    io:format("Processing Multicast message from ~p ~n", [McastMsg]),
	    #server_state{myid=Me, timestamp=MyTimeStamp, acklist=PrevAckList, registered_list = RegisteredList, msg_tracker = MessageTrackingList} = ServerState,

	    %%update message tracker
	    {HostId, LastMId} = lists:keyfind(HostId, 1, MessageTrackingList),
	    NewMessageTrackingList = case MId > LastMId of
					 true ->
					     lists:keystore(HostId, 1, MessageTrackingList, {HostId, MId});
					 false ->
					     MessageTrackingList
				     end,

	    %% updating my own timestamp
	    NewTimeStamp1 = case MsgTimeStamp > MyTimeStamp of
				true ->
				    MsgTimeStamp + 1;
				false ->
				    MyTimeStamp + 1
			    end,

	    AckList = [{ack,NodeId,HostId,MId} || #host_info{nodeid=NodeId,status=Status} <- RegisteredList, Status =:= player],
	    AllAckList = AckList ++ PrevAckList,
	    %% send your acks
	    message_passer:broadcast({ack,Me,HostId,MId}),

	    %% add to hold queue
	    HQ = ServerState#server_state.hold_queue,
	    NewHQ = [McastMsg | HQ],

	    %% sort the hold queue, based on the logical timestamps
	    SortedHQ = lists:sort(fun compare/2, NewHQ),

	    io:format("Contents of Hold queue ~p~n", [SortedHQ]),

	    loop(ServerState#server_state{acklist=AllAckList, timestamp=NewTimeStamp1, hold_queue = SortedHQ, msg_tracker = NewMessageTrackingList});
	{ack, _AckNode, Source, MsgId} = Ack ->
	    io:format("Received ack ~p~n", [Ack]),

	    MessageTrackingList = ServerState#server_state.msg_tracker,
	    {Source, LastMId} = lists:keyfind(Source, 1, MessageTrackingList),
	    case MsgId > LastMId of
		true ->
		    %% this means you havent yet received the relevant multicast message
		    %% so send it to ourself later
		    erlang:send_after(20,self(), Ack),
		    loop(ServerState);
		false ->
		    AckList = ServerState#server_state.acklist,
		    NewAckList = AckList -- [Ack],
		    HoldQueue = ServerState#server_state.hold_queue,
		    %% if newacklist is empty
		    %% BUGGGG: fix a bug here with getting the message from the head of the hold queue
		    case find_source_message({Source,MsgId}, NewAckList) of

			found ->
			    io:format("Waiting for acks from the Registered List~n"),
			    loop(ServerState#server_state{acklist = NewAckList});
			not_found ->
			    io:format("Processing the message~n"),
			    case HoldQueue of
				[] ->
				    %% holdqueue is empty so just loop back around and ignore ack
				    loop(ServerState#server_state{acklist=NewAckList});
				[{rmulti, HostId, MId, Msg, MsgTimeStamp} | Rest] ->
				    %%		    Head = [McastMsg3 | HoldQueue],
				    %% check if source and msgid field in mcastmsg3 is equal to Source, MsgId variables
				    NewHoldQueue = case (MId =:= MsgId) and (Source =:= HostId) of
						       true ->
							   %% process the message
							   case Msg of 
							       {lockRequest,ResourceId} ->
								   io:format("Forwarding lock Request~n"),
								   message_passer ! {lockRequest,HostId,MsgTimeStamp,ResourceId};
							       Any ->
								   io:format("Normal Multicast : ~p~n",[Any]),
								   route_message(Source, Any)
							   end,
							   Rest;
						       false ->
							   HoldQueue
						   end,
				    loop(ServerState#server_state{acklist=NewAckList,hold_queue = NewHoldQueue})
			    end
		    end
	    end;

	%% locking patterns

	{getLock, Pid, ResourceId} ->
	    io:format("CAlling Getlock for ~p~n", [ResourceId]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    MyNodeId = ServerState#server_state.myid,
	    RegisteredList = ServerState#server_state.registered_list,
	    case lists:keyfind(ResourceId, 1, LockStateList) of
		false ->
		    ReplyList = [{lockReply, Node, MyNodeId} || #host_info{nodeid=Node,status=Status} <- RegisteredList, Status =:= player],
		    NewLockStateList = lists:keystore(ResourceId, 1, LockStateList,
						      {ResourceId, released, queue:new(), ReplyList, Pid}),
		    LockMessage = {lockRequest, ResourceId},
		    message_passer ! {multicast,LockMessage},
		    loop(ServerState#server_state{lock_state_list = NewLockStateList});
		_Any ->
		    %% the reply list is not empty
		    %% reply with failure
		    Pid ! {failed, message_passer, ResourceId, {error, duplicate_request, ResourceId}},
		    loop(ServerState)
	    end;
	{releaseLock, Pid, ResourceId} ->
	    io:format("CAlling releaseLock for ~p~n", [ResourceId]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    MyNodeId = ServerState#server_state.myid,
	    case lists:keyfind(ResourceId, 1, LockStateList) of
		{ResourceId, held, RequestQueue, [], Pid} ->
		    lists:foreach(fun (RequestMsg) ->
					  {lockRequest, HostId, _ReqTimeStamp, ResourceId} = RequestMsg,
					  LockReply = {lockReply, MyNodeId, HostId, ResourceId},
					  message_passer ! {unicast, HostId, LockReply}
				  end,
				  queue:to_list(RequestQueue)),
		    NewLockStateList = lists:keydelete(ResourceId, 1, LockStateList),
		    Pid ! {released, message_passer, ResourceId},
		    loop(ServerState#server_state{lock_state_list = NewLockStateList});
		false ->
		    Pid ! {failed, message_passer, {error, lock_not_held, ResourceId}},
		    loop(ServerState)
	    end;
 	{lockRequest, HostId, ReqTimeStamp, ResourceId} = RequestMsg ->
	    %%	    {resourceid, lock_state}
	    io:format("Received lockRequest ~p~n", [RequestMsg]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    {ResourceId, LockState, RequestQueue, ReplyList, Pid} =
		case lists:keyfind(ResourceId, 1, LockStateList) of
		    {ResourceId, _, _, _, _} = Item ->
			Item;
		    false ->
			{ResourceId, released, queue:new(), [], none}
		end,
	    MyNodeId = ServerState#server_state.myid,
	    case {HostId, LockState} of
		{MyNodeId, held} ->
		    %% this is me.. should be there
		    loop(ServerState);
		{MyNodeId, released} ->
		    %% this is me so change to wanted
		    NewLockStateList = lists:keystore(ResourceId, 1, LockStateList,
						      {ResourceId, {wanted, ReqTimeStamp}, RequestQueue, ReplyList, Pid}),
		    ReplyMessage = {lockReply,MyNodeId,HostId,ResourceId},
		    message_passer ! {unicast,HostId,ReplyMessage},
		    loop(ServerState#server_state{lock_state_list = NewLockStateList});
		{_, released} ->
		    %% reply immediately
		    ReplyMessage = {lockReply,MyNodeId,HostId,ResourceId},
		    message_passer ! {unicast,HostId,ReplyMessage},
		    loop(ServerState);
		{_, held} ->
		    %% add the request to my request queue
		    NewRequestQueue = queue:in(RequestMsg, RequestQueue),
		    NewLockStateList = lists:keystore(ResourceId, 1, LockStateList,
						      {ResourceId, LockState, NewRequestQueue, ReplyList, Pid}),
		    loop(ServerState#server_state{lock_state_list=NewLockStateList});
		{_, {wanted, MyReqTimeStamp}} ->
		    %% compare timestamps
		    case compare({HostId,ReqTimeStamp},{MyNodeId,MyReqTimeStamp}) of
			true ->
			    %% a < b
			    ReplyMessage = {lockReply,MyNodeId,HostId,ResourceId},
			    message_passer ! {unicast, HostId ,ReplyMessage},
			    loop(ServerState);
			false ->
			    %% a > b
			    NewRequestQueue = queue:in(RequestMsg, RequestQueue),
			    NewLockStateList = lists:keystore(ResourceId, 1, LockStateList,
							      {ResourceId, LockState, NewRequestQueue, ReplyList, Pid}),
			    loop(ServerState#server_state{lock_state_list=NewLockStateList})
		    end
	    end;
 	{lockReply, NodeId, HostId, ResourceId} ->
	    %% for each node in the hostlist, check if OK reply received,
	    %% if true, takelock(),else loop back
	    io:format("Received a lockReply from ~p~n", [NodeId]),
	    LockStateList = ServerState#server_state.lock_state_list,
	    {ResourceId, LockState, RequestQueue, ReplyList, ReqPid} = lists:keyfind(ResourceId, 1, LockStateList),
	    LReply2 = [{lockReply, NodeId, HostId}],
	    NewReplyList = ReplyList -- LReply2,

	    case NewReplyList of
		[] ->
		    NewLockStateList = lists:keystore(ResourceId, 1, LockStateList, {ResourceId, held, queue:new(), [], ReqPid}),
		    %%  tell requestor of lock that we have acquired lock
		    ReqPid ! {acquired, message_passer, ResourceId},
		    loop(ServerState#server_state{lock_state_list = NewLockStateList});
		_NotEmpty ->
		    NewLockStateList = lists:keystore(ResourceId, 1, LockStateList,
						      {ResourceId, LockState, RequestQueue, NewReplyList, ReqPid}),
		    loop(ServerState#server_state{lock_state_list = NewLockStateList})
	    end;

	%% debugging patterns
	{become, Mod, NewLoopFun} ->
	    io:format("Running a new loop function ~p:~p~n", [Mod, NewLoopFun]),
	    %% check whether newloop is a real function before calling it.
	    {current_function, {_Module, _Fun, Arity}}  = erlang:process_info(self(), current_function),
	    case erlang:function_exported(Mod, NewLoopFun, Arity) of
		true ->
		    apply(Mod,NewLoopFun,[ServerState]);
		false ->
		    loop(ServerState)
	    end;
	{die} ->
	    io:format("message_passer closing...~n");
	{debug, Pid} ->
	    Pid ! {message_passer, ServerState},
	    loop(ServerState);
	{host_info, Pid, NodeId} ->
	    RegisteredList = ServerState#server_state.registered_list,
	    case lists:keyfind(NodeId, #host_info.nodeid, RegisteredList) of
		#host_info{host=Host, port=Port} ->
		    Pid ! {message_passer, {NodeId, Host, Port}};
		false ->
		    Pid ! {message_passer, {error, {node_not_found, NodeId}}}
	    end,
	    loop(ServerState);
	{broadcastping, Seq} ->
	    MyId = ServerState#server_state.myid,
	    message_passer ! {broadcast, {ping, MyId, Seq}},
	    loop(ServerState);
	{ping, HostId, Seq} ->
	    MyId = ServerState#server_state.myid,
	    message_passer ! {unicast, HostId, {pong,MyId,Seq}},
	    loop(ServerState);
	{pong, _, _} = Pong ->
	    pong_listener ! Pong,
	    loop(ServerState);
	{reset_server, _Pid} ->
	    self() ! {kill_comm},
	    loop(ServerState#server_state{acklist = [], hold_queue = []});

	{make_player, NodeId} ->
	    RegisteredList = ServerState#server_state.registered_list,
	    NewRegisteredList = case lists:keyfind(NodeId, #host_info.nodeid, RegisteredList) of
				    #host_info{status=guest} = HostInfo ->
					io:format("Making ~p a player~n", [NodeId]),
					HostInfo1 = HostInfo#host_info{status=player},
					lists:keystore(NodeId, #host_info.nodeid, RegisteredList, HostInfo1);
				    #host_info{status=player} ->
					io:format("~p is already a player~n", [NodeId]),
					RegisteredList;
				    false ->
					io:format("Player Id ~p is not found~n", [NodeId]),
					RegisteredList
				end,
	    loop(ServerState#server_state{registered_list=NewRegisteredList});
	Any ->
	    io:format("Ignoring unmatched message ~p~n", [Any]),
	    loop(ServerState)
    end.

reload_message_passer() ->
    code:load_file(message_passer),
    message_passer ! {become, message_passer, loop},
    ok.

debug() ->
    message_passer ! {debug, self()},
    receive
	{message_passer, Any} ->
	    Any
    end.

get_host_info(NodeId) ->
    message_passer ! {host_info, self(), NodeId},
    receive
	{message_passer, Any} ->
	    Any
    end.

reset_server_state() ->
    message_passer ! {reset_server, self()}.

kill_comm_processes() ->
    message_passer ! {kill_comm}.

get_node_info() ->
    message_passer ! {getinfo, self()},
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
    io:format("Number of Messages Received ~p~n", [Sum]),
    unregister(pong_listener);
	    
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
