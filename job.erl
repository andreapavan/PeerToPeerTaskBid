-module(job).
-export([addJob/6, listJob/0, getJobDetail/1, updateJobStatus/2, getFirstReadyJob/0, cleanJob/0]).

-include_lib("includes/record_definition.hrl").

% addJob(Status, Owner, Core, Ram, Disk, JobCost)
% {Status} as String
% {Owner} as Node (ex. 'node@machine')
% {Core, Ram, Disk, JobCost} as integer
addJob(Status, Owner, Core, Ram, Disk, JobCost) ->
	try
		JobInfo1 = #job_info{status=Status, owner=Owner, core=Core, ram=Ram, disk=Disk, job_cost=JobCost},
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

% updateJobStatus(Key, Status)
% {Key} as Binary
% {Status} as String
updateJobStatus(Key, Status) ->
	try
		% get detail of Job
		Obj = job:getJobDetail(Key),
		% update the job only if the status is not completed -> this is a final status and a job is not editable when this status is reached!!
		if Obj#job_info.status /= "completed" -> 
			UpdJpb = #job_info{status=Status, 
				owner=Obj#job_info.owner, 
				core=Obj#job_info.core, 
				ram=Obj#job_info.ram, 
				disk=Obj#job_info.disk,
				job_cost=Obj#job_info.job_cost},
			riak:updateObject('Job', Key, term_to_binary(UpdJpb))
		end
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% getFirstReadyJob()
getFirstReadyJob() ->
	{_, JobList} = job:listJob(),
	getFirstReadyJob(JobList).

% getFirstReadyJob(H|T)
% {H|T} as List in Head and Tail format
getFirstReadyJob([H|T]) -> 
	% get detail of Job
	Obj = job:getJobDetail(H),
	% get the job only if status is not complete and running
	if Obj#job_info.status /= "completed",
		Obj#job_info.status /= "running",
		Obj#job_info.owner == node() ->
		H;
	true -> 
		getFirstReadyJob(T)
	end;
getFirstReadyJob([]) -> false.


% --
% ------ DEBUG METHODS ------
% --


% cleanJob()
% cleans all Job recursively
cleanJob() ->
	{_, JobList} = job:listJob(),
	doCleanJob(JobList).

% doCleanJob([H|T]) 
% {H|T} as List in Head and Tail format
% recursive cleaning of Nodes
doCleanJob([H|T]) -> riak:deleteObject('Job', H), doCleanJob(T);
doCleanJob([]) -> true.

