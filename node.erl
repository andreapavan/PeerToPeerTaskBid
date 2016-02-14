-module(node).
-export([addNode/6, listNode/0, getNodeDetail/1, updateNode/6]).

-record(node_info,
	{status="",
	core=1,
	ram=1,
	disk=1,
	price=1}).

% addNode(Name, Status, Core, Ram, Disk, Price)
% {Name} as Node (ex. 'node@machine')
% {Status} as String
% {Core, Ram, Disk} as integer
addNode(Name, Status, Core, Ram, Disk, Price) ->
	try
		NodeInfo = #node_info{status=Status, core=Core, ram=Ram, disk=Disk, price=Price},
		riak:setObject('Node', atom_to_binary(Name, latin1), term_to_binary(NodeInfo))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% listNode()
listNode() ->
	try
		riak:listObject('Node')
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% getNodeDetail(Key)
% {Key} as Binary
getNodeDetail(Key) ->
	try
		{ok, Obj} = riak:getObject('Node', Key),
		Obj2 = binary_to_term(riakc_obj:get_value(Obj))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

% updateNode(Key, Status, Core, Ram, Disk, Price)
% {Status} as String
% {Core, Ram, Disk} as integer
updateNode(Key, Status, Core, Ram, Disk, Price) ->
	try
		Obj = node:getNodeDetail(Key),
		UpdNode = #node_info{status=Status, core=Core, ram=Ram, disk=Disk, price=Price},
		riak:updateObject('Node', Key, term_to_binary(UpdNode))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

