-module(policy).
-export([computeWorker/1]).

-record(node_info,
	{status="",
	core=1,
	ram=1,
	disk=1,
	price=1}).

-record(job_info,
	{status="",
	owner="",
	core=1,
	ram=1,
	disk=1,
	job_cost=1}).

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
	CurNode#node_info.core >= Job#job_info.core,
	CurNode#node_info.ram >= Job#job_info.ram,
	CurNode#node_info.disk >= Job#job_info.disk ->
		verifyNode(Job, T, [H|ValidNodes]);
	true ->
		verifyNode(Job, T, ValidNodes)
end;
verifyNode(_, [], ValidNodes) -> ValidNodes.

findBestBidder([H|T]) -> findBestBidder(H,T);
findBestBidder([]) -> null.

findBestBidder(M,[H|L]) when M#node_info.price < H#node_info.price -> findBestBidder(M,L);
findBestBidder(_,[H|L]) -> findBestBidder(H,L);
findBestBidder(M,[]) -> M.
