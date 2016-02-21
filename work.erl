-module(work).
-compile(export_all).

-include_lib("includes/record_definition.hrl").
 
start() ->
	try
  		register(node(), spawn(node(), ?MODULE, waitingForMessage, []))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


stop() ->
	try
  		exit(self(), kill)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


% handling incoming messages
waitingForMessage() ->
	receive
	{start, NodeTo, JobKey}->
      		io:format("Start working on job id ~p for node ~p.~n", [JobKey, NodeTo]),
		% update myself: remove resources used by the assigned job and set myself to working status
		workerAllocateResources(JobKey),
		% job runnin
		job:updateJobStatus(JobKey, "running"),
		% monitor the owner of the job
		main:monitorNode(NodeTo, JobKey),
		work:waitingForMessage();
	{cancel, NodeTo, JobKey}->
      		io:format("Canceled job id ~p for node ~p.~n", [JobKey, NodeTo]),
		work:waitingForMessage();
	{down, NodeDown, JobKey}->
      		io:format("The node ~p working on job id ~p is DOWN!!!!!!!!! ~n", [NodeDown, JobKey]),
		node:updateNodeStatus(atom_to_binary(NodeDown, latin1), "down"),
		job:updateJobStatus(JobKey, "down"),
		workerFreeResources(JobKey),
		work:waitingForMessage();
	{complete, NodeTo, JobKey}->
      		io:format("Completed job id ~p for node ~p.~n", [JobKey, NodeTo]),
		work:waitingForMessage();
    	Oops ->
      		io:format("Oops received: ~p~n", [Oops]),
      		work:waitingForMessage()
	end.

sendStartWork(NodeTo, JobKey) ->
	% send can be called only by owner

	{NodeTo#node_info.key, NodeTo#node_info.key} ! {start, node(), JobKey}.

sendCancelWork(NodeTo, JobKey) -> 
	% cancel can be called only by worker

	workerFreeResources(JobKey),
	job:updateJobStatus(JobKey, "ready"),
	{NodeTo, NodeTo} ! {cancel, node(), JobKey}.

sendDownWork(NodeDown, JobKey) ->
	% down must be called only by the system
	{node(), node()} ! {down, NodeDown, JobKey}.

sendCompleteWork(NodeTo, JobKey) -> 
	% complete can be called only by worker
	workerFreeResources(JobKey),
	job:updateJobStatus(JobKey, "completed"),
	{NodeTo, NodeTo} ! {complete, node(), JobKey}.
	
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

