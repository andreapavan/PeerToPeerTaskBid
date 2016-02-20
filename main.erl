-module(main).
-export([join/4, submitJob/4, checkJob/1, startJob/0, cleanDHT/0]).


% join(Core, Ram, Disk, Price)
% joins the job scheduling network by registering the node
% {Core, Ram, Disk, Price} as integer
join(Core, Ram, Disk, Price)-> 
	% connect to riak
	connect(),
	% register node to riak (status is ready by default)
	node:addNode(node(), "ready", Core, Ram, Disk, Price),
	% register node name as worker
	work:start(),
	io:format("Benvenuto ~p ~n", [node()]).

submitJob(Core, Ram, Disk, JobCost)-> 
	% register a new job to riak (status is ready by default)
	{_, {_, _, JobKey, _, _, _, _}} = job:addJob("ready", node(), Core, Ram, Disk, JobCost),
	% check if there is a node able to run the new job
	checkJob(JobKey).

checkJob(JobKey) ->
	% check if there is a node able to run an existing job
	Node = policy:computeWorker(JobKey),
	monitorNode(binary_to_atom(Node, latin1), JobKey).

monitorNode(null, JobKey) -> 
	io:format("Nessun nodo attulmente disponibile ~nRiprova piu tardi jobKey: ~p", [JobKey]),
	JobKey;

monitorNode(NodeName, _JobKey)->
	% Spawn a new process to receive the message from monitoring
	spawn(fun()->
		erlang:monitor_node(NodeName, true),
		% receive messages
		receive
			% if node is down ... 
			{nodedown, NodeDown} -> io:format("Nodo morto~n", []),
			work:sendDownWork(NodeDown, '123')
		end
	end).


startJob() ->
	io:format("Job iniziato~n", []).

% cleanDHT()
% cleans the DHT from all the values, used only for DEBUG purposes
cleanDHT() ->
	% connect to riak
	connect(),
	% clean all jobs
	job:cleanJob(),
	% clean all nodes
	node:cleanNode().

connect() ->
	try
		% get extra parameters for riak connection: address and port
		[RiakAddress,RiakPort] = init:get_plain_arguments(),
		{NewPort, _} = string:to_integer(RiakPort),
		% connection to riak node
		riak:start(RiakAddress, NewPort)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.
