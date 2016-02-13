-module(main).
-export([join/0, submitJob/4]).


join()-> 
	[RiakAddress,RiakPort] = init:get_plain_arguments(),
	{NewPort, _} = string:to_integer(RiakPort),
	riak:start(RiakAddress, NewPort).

submitJob(Core, Ram, Disk, JobCost)-> {ok}.
	%%job:addJob('andrea2@home', Core, Ram, Disk, JobCost).
