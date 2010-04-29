%%% File    : tcp_comm.erl
%%% Author  : ROAM
%%% Description : A tcp communcation module
%%% Created : 31 Mar 2010 

-module(tcp_comm).
-compile([export_all]).
-include("common.hrl").

-record(comm_state,
       {socket,
       myseqno=0,
       otherseqno=-1}).

start_client(Host, Port) ->
    spawn(tcp_comm, client_start, [Host, Port]).

client_connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}]) of
	{ok, Socket}-> Socket;
	{error, Reason}->?LOG("Error on connect Socket ~p. Trying again~n", [Reason]), 
			 client_connect(Host, Port)
    end.	

client_start({127,0,0,1}, Port) ->
    message_passer ! {comm_started, self()},
    send_node_id(),
    self_comm_loop(Port);

client_start(Host, Port) ->
    message_passer ! {comm_started, self()},
    Socket = client_connect(Host, Port),		
     %% send your node id to the server
    send_node_id(),
    comm_loop(#comm_state{socket=Socket}).

start_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,
					 {packet,4},
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
    %%?LOG("Sending message: ~p~n", [Msg]),
    Pid ! {send, self(), Msg},
    receive
	{Pid, Any} ->
	    Any
    end.

disconnect(Pid) ->
    Pid ! {disconnect},
    done.
    
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

comm_loop(#comm_state{socket=Socket, myseqno=SeqNo} = CommState) ->
    %% client should immediately receive the nodeid and forward it to the message passer
    receive
	{tcp, Socket, Data} ->
	    %% forward to message passer
	    Data1 = binary_to_term(Data),
	    message_passer ! {recvdata, self(), Data1},
	    comm_loop(CommState);
	{tcp_closed, Socket} ->
	    %% this may be an error.  should we try to reconnect or just tell message
	    %% passer that we are going to die?
	    ?LOG("Socket ~p closed~n", [Socket]),
	    %% this exit tells message passer that we are dying.  message passer will handle the rest
	    exit(socketclosed);
	{send, Pid, Data} ->
	    %%?LOG("Sending message ~p [~p to ~p]~n", [Data, inet:sockname(Socket), inet:peername(Socket)]),
	    BinData = term_to_binary(Data),
	    Result = gen_tcp:send(Socket, BinData),
	    Pid ! {self(), Result},
	    comm_loop(CommState#comm_state{myseqno=SeqNo+1});
	{disconnect} ->
	    %% close socket and finish loopp
	    gen_tcp:close(Socket);
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

self_comm_loop(Port) ->
    receive
	{send, Pid, Data} ->
	    message_passer ! {recvdata, self(), Data},
	    Pid ! {self(), ok},
	    self_comm_loop(Port);
	{getip, Pid} ->
	    Pid ! {self(), {127,0,0,1}},
	    self_comm_loop(Port);
	{disconnect} ->
	    %% ignore disconnect call and in fact reconnect to yourself
	    send_node_id(),
	    self_comm_loop(Port);
	{debug} ->
	    ?LOG("Debug info for self loop:~p", [Port]),
	    comm_loop(Port)
    end.
