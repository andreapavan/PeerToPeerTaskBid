-module(work).
-define(NODE_AS, 'daniele@127.0.0.1').
-define(NODE_TP, 'daniele2@127.0.0.1').
-compile(export_all).
 
start_ping() ->
	net_adm:ping(?NODE_TP),
  	register(?NODE_AS, spawn(?NODE_AS, ?MODULE, loop_ping, [])).
 
start_pong() ->
	net_adm:ping(?NODE_AS),
  	register(?NODE_TP, spawn(?NODE_TP,?MODULE, loop_pong, [])).
 
start_pinging() ->
  io:format("Pinging started ~n", []),
   ?NODE_AS ! {ping, {?NODE_AS, ?NODE_TP}, 0}.
 
stop({PidPing, PidPong}) ->
  exit(PidPing, normal),
  exit(PidPong, normal).
 
loop_ping() ->
  receive
    {ping, {PidPing, PidPong}, Number} when Number < 10  ->
      io:format("Ping received ~p on ~p~n", [Number, PidPing]),
      io:format("Nodes: ~p ~p ~n", [?NODE_AS, ?NODE_TP]),
      {?NODE_TP, ?NODE_TP} ! {pong, {PidPing, PidPong}, Number + 1},
      io:format("Ping sent ping ~p to ~p~n", [Number, PidPong]),
      work:loop_ping();
    {ping, {PidPing, PidPong}, Number} when Number >= 10->
      io:format("Ping received ~p on ~p~n and closing.~n", [Number, PidPing]);
    Oops ->
      io:format("Oops received: ~p~n", [Oops]),
      work:loop_ping()
  end.
 
loop_pong() ->
  receive
   {pong, {PidPing, PidPong}, Number} when Number < 10  ->
     io:format("Pong received ~p on ~p~n", [Number, PidPong]),
     io:format("Nodes: ~p ~p ~n", [?NODE_AS, ?NODE_TP]),
     {?NODE_AS, ?NODE_AS} ! {ping, {PidPing, PidPong}, Number + 1},
     io:format("Pong sent ping ~p to ~p~n", [Number, PidPing]),
     work:loop_pong();
   {pong, {PidPing, PidPong}, Number} when Number >= 10->
     io:format("Pong received ~p on ~p~n and closing.~n", [Number, PidPing]);
   Oops ->
     io:format("Oops received: ~p~n", [Oops]),
     work:loop_pong()
  end.
