-module(message).
-behaviour(gen_server).

%% Export node functions
-export([connect/0, disconnect/0, newWorker/2, startJob/2, completeJob/2, cancelJob/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile(export_all).

connect() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
disconnect() -> gen_server:call(?MODULE, stop).

newWorker(Who,JobId) -> gen_server:call(?MODULE, {new,Who,JobId}).
cancelJob(Who,JobId) -> gen_server:call(?MODULE, {cancel,Who,JobId}).
startJob(Who,JobId) -> gen_server:call(?MODULE, {start,Who,JobId}).
completeJob(Who,JobId) -> gen_server:call(?MODULE, {complete,Who,JobId}).

init([]) -> {ok, ets:new(?MODULE,[])}.

handle_call({new,Who,_JobId}, From, Tab) ->
	try
		Reply = case ets:lookup(Tab, Who) of
			[]  -> ets:insert(Tab, {Who,0}), 
		    	   {welcome, Who, From};
			[_] -> {Who, you_already_are_a_worker}
	    	end,
	    	{reply, Reply, Tab}
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end;

handle_call({cancel,Who,_JobId}, _From, Tab) ->
	try
		Reply = case ets:lookup(Tab, Who) of
			[_] -> ets:delete(Tab, Who),
					{thanks_for_the_work, Who};
			[]  ->  {you_are_not_a_worker, Who}
	    	end,
    	{reply, Reply, Tab}
    catch
		Exception:Reason -> {caught, Exception, Reason}
	end;

handle_call({start,Who,_JobId}, From, Tab) ->
	try
		Reply = {start, Who, From},
    	{reply, Reply, Tab}
    catch
		Exception:Reason -> {caught, Exception, Reason}
	end;

handle_call({complete,Who,_JobId}, From, Tab) ->
	try
		Reply = {complete, Who, From},
    	{reply, Reply, Tab}
    catch
		Exception:Reason -> {caught, Exception, Reason}
	end;

handle_call(stop, _From, Tab) ->
	try
    	{stop, normal, stopped, Tab}
    catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
