-module(node).
-export([addNode/6, listNode/0, getNodeDetail/1, updateNode/6, updateNodeStatus/2, cleanNode/0]).

-include_lib("includes/record_definition.hrl"). 

% addNode(Name, Status, Core, Ram, Disk, Price)
% {Name} as Node (ex. 'node@machine')
% {Status} as String
% {Core, Ram, Disk} as integer
addNode(Name, Status, Core, Ram, Disk, Price) ->
	try
		NodeInfo = #node_info{key=Name, status=Status, core=Core, ram=Ram, disk=Disk, price=Price},
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
		UpdNode = #node_info{key=Obj#node_info.key, status=Status, core=Core, ram=Ram, disk=Disk, price=Price},
		riak:updateObject('Node', Key, term_to_binary(UpdNode))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


% updateNodeStatus(Key, Status)
% {Key} as Binary
% {Status} as String
updateNodeStatus(Key, Status) ->
	try
		Obj = node:getNodeDetail(Key),
		UpdNode = #node_info{key=Obj#node_info.key, status=Status, core=Obj#node_info.core, ram=Obj#node_info.ram, disk=Obj#node_info.disk, price=Obj#node_info.price},
		riak:updateObject('Node', Key, term_to_binary(UpdNode))
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


% --
% ------ DEBUG METHODS ------
% --


% cleanNode()
% cleans all Node recursively
cleanNode() ->
	{_, NodeList} = node:listNode(),
	doCleanNode(NodeList).

% doCleanNode([H|T]) 
% {H|T} as List in Head and Tail format
% recursive cleaning of Nodes
doCleanNode([H|T]) -> riak:deleteObject('Node', H), doCleanNode(T);
doCleanNode([]) -> true.

