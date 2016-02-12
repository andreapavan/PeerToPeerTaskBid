-module(riak).
-export([testLib/0, start/2, setObject/3, getObject/2, updateObject/3, deleteObject/2, listObject/1]).

% Check the library for Riak Client
testLib() -> code:which(riakc_pb_socket).

% Start connection with Riak Node identified by {Host, Port}
start(Host, Port) ->
	try
		{ok, Pid} = riakc_pb_socket:start_link(Host, Port),
		ets:new(config_table, [named_table, protected, set, {keypos, 1}]),
	  	ets:insert(config_table, {pid, Pid})
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% setObject(Bucket, Key, Value)
% {Bucket, Key, Value} as string
setObject(Bucket, Key, Value) ->
	try
		[{_, Pid}] = ets:lookup(config_table, pid),
		Obj = riakc_obj:new(atom_to_binary(Bucket, latin1), Key, Value),
		riakc_pb_socket:put(Pid, Obj)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% getObject(Bucket, Key)
% {Bucket, Key} as string
getObject(Bucket, Key) ->
	try
		[{_, Pid}] = ets:lookup(config_table, pid),
		{ok, Obj} = riakc_pb_socket:get(Pid, atom_to_binary(Bucket, latin1), Key)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% deleteObject(Bucket, Key)
% {Bucket, Key} as string
deleteObject(Bucket, Key) ->
	try
		[{_, Pid}] = ets:lookup(config_table, pid),
		riakc_pb_socket:delete(Pid, atom_to_binary(Bucket, latin1), Key)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% updateObject(Bucket, Key, NewValue)
% {Bucket, Key, NewValue} as string
updateObject(Bucket, Key, NewValue) ->
	try
		[{_, Pid}] = ets:lookup(config_table, pid),
		{ok, Oa} = riakc_pb_socket:get(Pid, atom_to_binary(Bucket, latin1), Key),
		Ob = riakc_obj:update_value(Oa, NewValue),
		{ok, Oc} = riakc_pb_socket:put(Pid, Ob, [return_body])
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% listObject(Bucket)
% {Bucket} as string
listObject(Bucket) ->
	try
		[{_, Pid}] = ets:lookup(config_table, pid),
		{ok, Obj} = riakc_pb_socket:list_keys(Pid, atom_to_binary(Bucket, latin1))
		
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.
