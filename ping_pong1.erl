-module(ping_pong1).
-export([start/0, saySomething/2, ping/2, pong/0]).

saySomething(_, 0) -> done;
saySomething(What, Times) ->
	io:format("~ts~n", [What]),
	saySomething(What, Times - 1).

% start() ->
% 	spawn(ping, saySomething, [hello, 3]),
% 	spawn(ping, saySomething, [goodbye, 5]).

ping(0, Pong_PID) ->
	Pong_PID ! finished,
	io:format("** Ping finished~n", []);

ping(N, Pong_PID) ->
	Pong_PID ! {ping, self()},
	receive
		pong ->
			io:format("Ping received~n", [])
	end,
	ping(N - 1, Pong_PID).

pong() ->
	receive
		finished ->
			io:format("++ Pong finished~n", []);
		{ping, Ping_PID} ->
			io:format("Pong received ping~n", []),
			Ping_PID ! pong,
			pong()
	end.

start() ->
	Pong_PID = spawn(ping_pong, pong, []),
	spawn(ping_pong, ping, [6, Pong_PID]).
