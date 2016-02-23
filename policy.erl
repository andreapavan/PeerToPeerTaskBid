-module(policy).
-export([computeWorker/1]).

-include_lib("includes/record_definition.hrl").


% computeWorker(Key)
% {Key} as Binary
computeWorker(Key) ->
	try	
		% get Job detail
		LastJob = job:getJobDetail(Key),
		% get all nodes
		{_, NodeList} = node:listNode(),
		% get the best nome with enough available resources (and not down)
		Nodes = verifyNode(LastJob, NodeList, []),
		% fine the node with the lowest price
		findBestBidder(Nodes)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


% verifyNode(Job, [H|T], ValidNodes)
% Job as record (job)
% {H|T} as List in Head and Tail format
% ValidNodes as List of record (node)
verifyNode(Job, [H|T], ValidNodes) ->
	% get detail of current Node
	CurNode = node:getNodeDetail(H),
	% if the node has enough available resoures, it's not myself (I can't execute my jobs) and it's not down
	if
		CurNode#node_info.key /= Job#job_info.owner,
		CurNode#node_info.status /= "down",
		CurNode#node_info.core >= Job#job_info.core,
		CurNode#node_info.ram >= Job#job_info.ram,
		CurNode#node_info.disk >= Job#job_info.disk ->
			verifyNode(Job, T, [CurNode|ValidNodes]);
		true ->
			verifyNode(Job, T, ValidNodes)
	end;
verifyNode(_, [], ValidNodes) -> ValidNodes.

% findBestBidder([H|T])
% {H|T} as List in Head and Tail format
findBestBidder([H|T]) -> findBestBidder(H,T);
findBestBidder([]) -> null.

% findBestBidder(M, [H|L])
% {M} as record (Node)
% {H|L} as List in Head and Tail format
findBestBidder(M,[H|L]) when M#node_info.price < H#node_info.price -> findBestBidder(M,L);
findBestBidder(_,[H|L]) -> findBestBidder(H,L);
findBestBidder(M,[]) -> M.

