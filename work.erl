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

 
waitingForMessage() ->
	receive
	{start, NodeTo, JobKey}->
      		io:format("Start working on job id ~p for node ~p.~n", [JobKey, NodeTo]),
		% update myself: remove resources used byt my job and set myself to working status
		JobObj = job:getJobDetail(JobKey),
		NodeObj = node:getNodeDetail(atom_to_binary(node(), latin1)),
		node:updateNode(atom_to_binary(node(), latin1), "working", 
			NodeObj#node_info.core - JobObj#job_info.core, 
			NodeObj#node_info.ram - JobObj#job_info.ram, 
			NodeObj#node_info.disk - JobObj#job_info.disk, 
			NodeObj#node_info.price),
		job:updateJobStatus(JobKey, "running"),
		main:monitorNode(NodeTo, JobKey),
		work:waitingForMessage();
	{cancel, NodeTo, JobKey}->
      		io:format("Canceled job id ~p for node ~p.~n", [JobKey, NodeTo]),
		work:waitingForMessage();
	{down, NodeDown, JobKey}->
      		io:format("The node ~p working on job id ~p is DOWN!!!!!!!!! ~n", [NodeDown, JobKey]),
		node:updateNodeStatus(atom_to_binary(NodeDown, latin1), "down"),
		job:updateJobStatus(JobKey, "down"),
		work:waitingForMessage();
	{complete, NodeTo, JobKey}->
      		io:format("Completed job id ~p for node ~p.~n", [JobKey, NodeTo]),
		work:waitingForMessage();
    	Oops ->
      		io:format("Oops received: ~p~n", [Oops]),
      		work:waitingForMessage()
	end.

sendStartWork(NodeTo, JobId) ->
	{NodeTo#node_info.key, NodeTo#node_info.key} ! {start, node(), JobId}.

sendCancelWork(NodeTo, JobId) -> 
	{NodeTo, NodeTo} ! {cancel, node(), JobId}.

sendDownWork(NodeDown, JobId) ->
	{node(), node()} ! {down, NodeDown, JobId}.

sendCompleteWork(NodeTo, JobId) -> 
	{NodeTo, NodeTo} ! {complete, node(), JobId}.
	

