-module(main).
-export([join/0, submitJob/4, monitorNode/1, startJob/0, cleanDHT/0]).


join()-> 
	[RiakAddress,RiakPort] = init:get_plain_arguments(),
	{NewPort, _} = string:to_integer(RiakPort),
	riak:start(RiakAddress, NewPort).

submitJob(_Core, _Ram, _Disk, _JobCost)-> {ok}.
	%%job:addJob('andrea2@home', Core, Ram, Disk, JobCost).

monitorNode(NodeName)->
	% Spawn a new process to receive the message from monitoring
	spawn(fun()->
		erlang:monitor_node(NodeName, true),
		% receive messages
		receive
			% if node is down ... 
			{nodedown, _} -> io:format("Nodo morto~n", [])
		end
	end).

startJob() ->
	io:format("Job iniziato~n", []).

% cleanDHT()
% cleans the DHT from all the values, used only for DEBUG purposes
cleanDHT() ->
	job:cleanJob(),
	node:cleanNode().
