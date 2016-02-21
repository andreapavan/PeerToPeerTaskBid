-module(policy).
-export([computeWorker/1]).

-include_lib("includes/record_definition.hrl").


computeWorker(Key) ->
	try
		LastJob = job:getJobDetail(Key),
		{_, NodeList} = node:listNode(),
		Nodes = verifyNode(LastJob, NodeList, []),
		findBestBidder(Nodes)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

verifyNode(Job, [H|T], ValidNodes) ->
CurNode = node:getNodeDetail(H),
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

findBestBidder([H|T]) -> findBestBidder(H,T);
findBestBidder([]) -> null.

findBestBidder(M,[H|L]) when M#node_info.price < H#node_info.price -> findBestBidder(M,L);
findBestBidder(_,[H|L]) -> findBestBidder(H,L);
findBestBidder(M,[]) -> M.
