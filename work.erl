-module(work).
-compile(export_all).
 
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
    	{new, NodeTo, JobKey}->
      		io:format("New job id ~p from node ~p.~n", [JobKey, NodeTo]),
		work:waitingForMessage();
	{start, NodeTo, JobKey}->
      		io:format("Start working on job id ~p for node ~p.~n", [JobKey, NodeTo]),
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

sendNewWork(NodeTo, JobId) -> 
	{NodeTo, NodeTo} ! {new, node(), JobId}.

sendStartWork(NodeTo, JobId) -> 
	{NodeTo, NodeTo} ! {start, node(), JobId}.

sendCancelWork(NodeTo, JobId) -> 
	{NodeTo, NodeTo} ! {cancel, node(), JobId}.

sendDownWork(NodeDown, JobId) ->
	{node(), node()} ! {down, NodeDown, JobId}.

sendCompleteWork(NodeTo, JobId) -> 
	{NodeTo, NodeTo} ! {complete, node(), JobId}.
	

