-module(work).
-compile(export_all).

-include_lib("includes/record_definition.hrl").

% start()
% register a new process with the name (same of the node) and listen to incoming messages
start() ->
	try
  		register(node(), spawn(node(), ?MODULE, waitingForMessage, []))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% stop()
% exits the current listening process by killig the processs
stop() ->
	try
  		exit(self(), kill)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


% waitingForMessage()
% handling incoming messages
waitingForMessage() ->
	receive
	{start, NodeTo, JobKey}->
      		io:format("Start working on job id ~p for node ~p.~n", [JobKey, NodeTo]),
		% update myself: remove resources used by the assigned job and set myself to working status
		workerAllocateResources(JobKey),
		% job running
		job:updateJobStatus(JobKey, "running"),
		% monitor the owner of the job so I can update Riak if it's going down
		main:monitorNode(NodeTo, JobKey),
		work:waitingForMessage();
	{cancel, NodeTo, JobKey}->
      		io:format("Canceled job id ~p for node ~p.~n", [JobKey, NodeTo]),
		work:waitingForMessage();
	{down, NodeDown, JobKey}->
      		io:format("The node ~p working on job id ~p is DOWN!!!!!!!!! ~n", [NodeDown, JobKey]),
		% the node is down, update Riak accordingly Node and Job informations
		node:updateNodeStatus(atom_to_binary(NodeDown, latin1), "down"),
		job:updateJobStatus(JobKey, "down"),
		% try to release resources (if I'm a worker)
		workerFreeResources(JobKey),
		work:waitingForMessage();
	{complete, NodeTo, JobKey}->
      		io:format("Completed job id ~p for node ~p.~n", [JobKey, NodeTo]),
		work:waitingForMessage();
    	Oops ->
      		io:format("Oops received: ~p~n", [Oops]),
      		work:waitingForMessage()
	end.

% sendStartWork(NodeTo, JobKey)
% --
% ---- start can be called only by owner ----
% --
sendStartWork(NodeTo, JobKey) ->
	{NodeTo#node_info.key, NodeTo#node_info.key} ! {start, node(), JobKey}.

% sendCancelWork(NodeTo, JobKey)
% --
% ---- cancel can be called only by worker ----
% --
sendCancelWork(NodeTo, JobKey) ->
	workerFreeResources(JobKey),
	% set Job as ready since I'm no longer working on this...
	job:updateJobStatus(JobKey, "ready"),
	{NodeTo, NodeTo} ! {cancel, node(), JobKey}.

% sendDownWork(NodeDown, JobKey)
% --
% ---- down must be called only by the system itself (automatic handled) ----
% --
sendDownWork(NodeDown, JobKey) ->
	% down must be called only by the system
	{node(), node()} ! {down, NodeDown, JobKey}.

% sendCompleteWork(NodeTo, JobKey)
% --
% ---- complete can be called only by worker ----
% --
sendCompleteWork(NodeTo, JobKey) -> 
	workerFreeResources(JobKey),
	% set Job as completed since I finished working on this
	job:updateJobStatus(JobKey, "completed"),
	{NodeTo, NodeTo} ! {complete, node(), JobKey}.

% workerAllocateResources(JobKey)
% {Job} as Binary
% allocate the resources of a worker (myself)
workerAllocateResources(JobKey) ->
	% update myself: remove resources used by the assigned job and set myself to working status
	JobObj = job:getJobDetail(JobKey),
	NodeObj = node:getNodeDetail(atom_to_binary(node(), latin1)),
	node:updateNode(atom_to_binary(node(), latin1), 
		"working", 
		NodeObj#node_info.core - JobObj#job_info.core, 
		NodeObj#node_info.ram - JobObj#job_info.ram, 
		NodeObj#node_info.disk - JobObj#job_info.disk, 
		NodeObj#node_info.price).

% workerFreeResources(JobKey)
% {Job} as Binary
% release the resources of a worker (myself)
workerFreeResources(JobKey) ->
	% update myself: add resources used by the assigned job
	JobObj = job:getJobDetail(JobKey),
	% only if the job owner is not myself (so I'm the worker)
	io:format("~p  ~p", [JobObj#job_info.owner, node()]),
	if JobObj#job_info.owner /= node() ->
		NodeObj = node:getNodeDetail(atom_to_binary(node(), latin1)),
		node:updateNode(atom_to_binary(node(), latin1), 
			"working", 
			NodeObj#node_info.core + JobObj#job_info.core, 
			NodeObj#node_info.ram + JobObj#job_info.ram, 
			NodeObj#node_info.disk + JobObj#job_info.disk, 
			NodeObj#node_info.price),
		io:format("Released resorces of your job~n", []);
	true -> 
		io:format("Resorces not released, you're the owner of this job~n", [])
	end.

