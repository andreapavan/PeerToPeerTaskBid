-module(job).
-export([addJob/5, listJob/0, getJobDetail/1]).

-record(job_info,
	{owner="",
	core=1,
	ram=1,
	disk=1,
	job_cost=1}).

% addJob(Owner, Core, Ram, Disk, JobCost)
% {Owner} as Node (ex. 'node@machine')
% {Core, Ram, Disk, JobCost} as integer
addJob(Owner, Core, Ram, Disk, JobCost) ->
	try
		JobInfo1 = #job_info{owner=Owner, core=Core, ram=Ram, disk=Disk, job_cost=JobCost},
		riak:setObject('Job', undefined, term_to_binary(JobInfo1))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% listJob()
listJob() ->
	try
		riak:listObject('Job')
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% getJobDetail(Key)
% {Key} as Binary
getJobDetail(Key) ->
	try
		{ok, Obj} = riak:getObject('Job', Key),
		Obj2 = binary_to_term(riakc_obj:get_value(Obj))

	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

