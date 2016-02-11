-module(riak).
-export([testLib/0, start/0, setObject/3, getObject/2, updateObject/3, deleteObject/2]).

testLib() -> code:which(riakc_pb_socket).

start() ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017).

setObject(Bucket, Key, Value) ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017),
	Obj = riakc_obj:new(atom_to_binary(Bucket, latin1), atom_to_binary(Key, latin1), atom_to_binary(Value, latin1)),
	riakc_pb_socket:put(Pid, Obj).

getObject(Bucket, Key) ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017),
	{ok, Obj} = riakc_pb_socket:get(Pid, atom_to_binary(Bucket, latin1), atom_to_binary(Key, latin1)).

deleteObject(Bucket, Key) ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017),
	riakc_pb_socket:delete(Pid, atom_to_binary(Bucket, latin1), atom_to_binary(Key, latin1)).

updateObject(Bucket, Key, NewValue) ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017),
	{ok, Oa} = riakc_pb_socket:get(Pid, atom_to_binary(Bucket, latin1), atom_to_binary(Key, latin1)),
	Ob = riakc_obj:update_value(Oa, atom_to_binary(NewValue, latin1)),
	{ok, Oc} = riakc_pb_socket:put(Pid, Ob, [return_body]).
