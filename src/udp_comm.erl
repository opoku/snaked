%%% File    : tcp_comm.erl
%%% Author  : ROAM
%%% Description : A tcp communcation module
%%% Created : 31 Mar 2010 

-module(udp_comm).
-compile([export_all]).
-include("common.hrl").


-record(comm_state,
       {socket,
	resendlist=[],
	myseqno=0,
	otherseqno=-1
       }).

start_client(Host, Port) ->
    spawn(udp_comm, client_start, [Host, Port]).

client_start(Host, Port) ->
    message_passer ! {comm_started, self()},
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    %% send your node id to the server
    send_node_id(),
    comm_loop(#comm_state{socket=Socket}).

start_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,
					 {packet,0},
					 {reuseaddr, true},
					 {active, true}]),
    spawn(tcp_comm, par_connect, [Port, Listen]).

par_connect(Port, Listen) ->
    ?LOG("Waiting for connection~n",[]),
    %% when message_passer dies this returns with {error, closed} so we have to handle
    %% this somewhere
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    ?LOG("Connected on Socket ~p~n", [Socket]),
	    spawn(tcp_comm, par_connect, [Port, Listen]),
	    %% send your node id to client
	    message_passer ! {comm_started, self()},
	    send_node_id(),
	    comm_loop(#comm_state{socket=Socket});
	{error, Reason} ->
	    ?LOG("Error on listen Socket ~p(~p). Not relistening~n", [Listen, Reason])
	    %%start_server(Port)
    end.

send_msg(Pid, Msg) ->
    Pid ! {send, self(), Msg},
    receive
	{Pid, Any} ->
	    Any
    end.
    
send_node_id() ->
    {MyNodeId, MyNodePort} = message_passer:get_node_info(),
    self() ! {send, self(), {register, MyNodeId, MyNodePort}},
    ok.

get_host_ip(Pid) ->
    Pid ! {getip, self()},
    receive
	{Pid, Ip} ->
	    Ip;
	{Pid, Reason} ->
	    Reason
    end.

resend_function(TimeOut, Pid, Data) ->
    receive
	cancel ->
	    done
    after
	TimeOut ->
	    Pid ! {resend, self(), Data},
	    receive
		_Any ->
		    done
	    end,
	    resend_function(TimeOut, Pid, Data)
    end.
		
	    

schedule_resend(Pid, Data) ->
    spawn(fun () ->
		  resend_function(50, Pid, Data)
	  end).

comm_loop(#comm_state{socket=Socket, myseqno=SeqNo, otherseqno=LastSeqNo, resendlist=ResendList} = CommState) ->
    %% client should immediately receive the nodeid and forward it to the message passer
    receive
	{tcp, Socket, Data} ->
	    %% forward to message passer
	    Data1 = binary_to_term(Data),
	    ?LOG("Received tcp data~p~n", [Data1]),
	    case Data1 of
		{ack, AckSeqNo} ->
		    case lists:keytake(AckSeqNo, 2, ResendList) of
			{value, {Pid, AckSeqNo}, NewResendList} ->
			    Pid ! cancel,
			    ?LOG("Cancelling resend for seqno~p~n", [AckSeqNo]),
			    comm_loop(CommState#comm_state{resendlist = NewResendList});
			false ->
			    ?LOG("Ignoring ack for seqno~p~n", [AckSeqNo]),
			    comm_loop(CommState)
		    end;
		{RecvSeqNo, Msg} ->
		    gen_tcp:send(Socket, term_to_binary({ack, RecvSeqNo})),
		    ?LOG("Ack sent for ~p~n", [RecvSeqNo]),
		    case RecvSeqNo > LastSeqNo of
			true ->
			    ?LOG("TCP received message ~p~n", [Msg]),
			    message_passer ! {recvdata, self(), Msg},
			    comm_loop(CommState#comm_state{otherseqno = RecvSeqNo});
			false ->
			    ?LOG("Duplicate message with ack ~p so ignoring~n", [RecvSeqNo]),
			    comm_loop(CommState)
		    end
	    end;
	{tcp_closed, Socket} ->
	    %% this may be an error.  should we try to reconnect or just tell message
	    %% passer that we are going to die?
	    ?LOG("Socket ~p closed~n", [Socket]),
	    %% this exit tells message passer that we are dying.  message passer will handle the rest
	    exit(socketclosed);
	{send, Pid, Data} ->
	    ?LOG("Sending message ~p with seqno~p [~p to ~p]~n", [Data, SeqNo, inet:sockname(Socket), inet:peername(Socket)]),
	    BinData = term_to_binary({SeqNo, Data}),
	    Result = gen_tcp:send(Socket, BinData),
	    Pid ! {self(), Result},

	    %% schedule a resend
	    ResendPid = schedule_resend(self(), BinData),
	    comm_loop(CommState#comm_state{myseqno=SeqNo+1, resendlist= [{ResendPid, SeqNo}| ResendList]});
	{resend, Pid, BinData} ->
	    Result = gen_tcp:send(Socket, BinData),
	    {ReSeqNo, _Msg} = binary_to_term(BinData),
	    ?LOG("Resending msg for seq~p on Socket ~p ~n", [ReSeqNo, Socket]),
	    Pid ! {self(), Result},
	    comm_loop(CommState);
	{getip, Pid} ->
	    case inet:peername(Socket) of
		{ok, {Address, _Port}} ->
		    Pid ! {self(), Address},
		    comm_loop(CommState);
		{error, Reason} ->
		    Pid ! {self(), Reason}
	    end;
	{debug} ->
	    ?LOG("Debug info:~p", [CommState]),
	    comm_loop(CommState)
    end.    
