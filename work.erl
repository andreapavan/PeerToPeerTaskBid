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
    	{new, NodeTo, JobId}->
      		io:format("New job id ~p from node ~p.~n", [JobId, NodeTo]),
		work:waitingForMessage();
	{start, NodeTo, JobId}->
      		io:format("Start working on job id ~p for node ~p.~n", [JobId, NodeTo]),
		work:waitingForMessage();
	{cancel, NodeTo, JobId}->
      		io:format("Canceled job id ~p for node ~p.~n", [JobId, NodeTo]),
		work:waitingForMessage();
	{down, NodeDown, JobId}->
      		io:format("The node ~p working on job id ~p is DOWN!!!!!!!!! ~n", [NodeDown, JobId]),
		work:waitingForMessage();
	{complete, NodeTo, JobId}->
      		io:format("Completed job id ~p for node ~p.~n", [JobId, NodeTo]),
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
	

