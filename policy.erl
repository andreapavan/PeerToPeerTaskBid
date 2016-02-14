-module(policy).

-export([computeWorker/1]).

-record(job_info,
	{owner="",
	core=1,
	ram=1,
	disk=1,
	job_cost=1}).

computeWorker(Key) ->
	try
		LastJob = job:getJobDetail(Key),
		{_, JobList} = job:listJob(),
		Nodes = verifyNode(LastJob, JobList, []),
		findBestBidder(Nodes)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

verifyNode(Job, [H|T], ValidNodes) ->
CurNode = job:getJobDetail(H),
if
	CurNode#job_info.core >= Job#job_info.core,
	CurNode#job_info.ram >= Job#job_info.disk,
	CurNode#job_info.disk >= Job#job_info.disk ->
		verifyNode(Job, T, [CurNode|ValidNodes]);
	true ->
		verifyNode(Job, T, ValidNodes)
end;
verifyNode(_, [], ValidNodes) -> ValidNodes.

findBestBidder([H|T]) -> findBestBidder(H,T).

findBestBidder(M,[H|L]) when M#job_info.job_cost < H#job_info.job_cost -> findBestBidder(M,L);
findBestBidder(_,[H|L]) -> findBestBidder(H,L);
findBestBidder(M,[]) -> M.