-module(main).
-export([join/4, submitJob/4, checkJob/0, checkJob/1, monitorNode/2, cleanDHT/0]).

-include_lib("includes/record_definition.hrl").


% join(Core, Ram, Disk, Price)
% {Core, Ram, Disk, Price} as integer
% joins the job scheduling network by registering the node
join(Core, Ram, Disk, Price)-> 
	% connect to riak
	connect(),
	% register node to riak (status is ready by default)
	node:addNode(node(), "ready", Core, Ram, Disk, Price),
	% register node name as worker
	work:start(),
	io:format("Welcome ~p ~n", [node()]).

% submitJob(Core, Ram, Disk, JobCost)
% {Core, Ram, Disk, JobCost} as Integer
% submit a new job to the system and attemp to assign a worker to it
submitJob(Core, Ram, Disk, JobCost)-> 
	% register a new job to riak (status is ready by default)
	{_, {_, _, JobKey, _, _, _, _}} = job:addJob("ready", node(), Core, Ram, Disk, JobCost),
	% check if there is a node able to run the new job
	checkJob(JobKey).

% checkJob()
% gets the first Job with myself as owner ready to be assigned to a node
checkJob() ->
	checkJob(job:getFirstReadyJob()).

% checkJob(JobKey)
% {JobKey} as Binary
% attemp to find a Worker for the Job
checkJob(false) -> 
	io:format("Invalid job ~n");
checkJob(JobKey) ->
	% get details of Job
	JobObj = job:getJobDetail(JobKey),

	% check if I'm the owner of the Job, if it's not running and it's not already been completed
	if JobObj#job_info.owner /= node() ->
		io:format("You're not the owner of this job ~n", []);
	JobObj#job_info.status == "running" ->
		io:format("The job is already running ~n", []);
	JobObj#job_info.status == "completed" ->
		io:format("The job is already completed ~n", []);
	true ->
		% check if there is a node able to run an existing job
		Node = policy:computeWorker(JobKey),
		if Node /= null ->
			monitorNode(Node#node_info.key, JobKey),
			startJob(Node, JobKey);
		true ->
			io:format("No node is currently available for this Job ~nTry later, this is the JobKey: ~p~n", [JobKey]),
			JobKey
		end
		
	end.
	

	
% monitorNode(Node, JobKey)
% {Node} as machine name
% {JobKey} as Binary
% start a monitor process that received the nodedown message if the Node is down and then updates Riak accordingly
% also start a new connection with the node, if connection cannot be started the nodedown message is returned
monitorNode(Node, JobKey)->
	% Spawn a new process to receive the message from monitoring
	spawn(fun()->
		erlang:monitor_node(Node, true),
		% receive messages
		receive
			% if node is down ...
			{nodedown, NodeDown} -> 
				work:sendDownWork(NodeDown, JobKey)
		end
	end).

% startJob(Node, JobKey)
% {Node} as Binary
% {JobKey} as Binary
% starts a new Job and update Riak accordingly
startJob(Node, JobKey) ->
	io:format("Job started!~n", []),
	work:sendStartWork(Node, JobKey).


% connect()
% attemp a connecion to Riak using the command line parameters
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


% --
% ------ DEBUG METHODS ------
% --


% cleanDHT()
% cleans the DHT from all the values, used only for DEBUG purposes
cleanDHT() ->
	% connect to riak
	connect(),
	% clean all jobs
	job:cleanJob(),
	% clean all nodes
	node:cleanNode().

