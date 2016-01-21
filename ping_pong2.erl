-module(ping_pong2).
-export([start/0, ping/1, pong/0]).

ping(0) ->
	pong ! finished,
	io:format("** Ping finished~n", []);

ping(N) ->
	pong ! {ping, self()},
	receive
		pong ->
			io:format("Ping received~n", [])
	end,
	ping(N - 1).

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
	register(pong, spawn(ping_pong2, pong, [])),
	spawn(ping_pong2, ping, [6]).
