-module(coordinator).

-include_lib("headers/records.hrl").

-export([handle/3]).


handle(Server, Nodes, Version) ->
    loop(Server, Nodes, Version).


loop(Server, Nodes, Version) ->
    receive 
        { node_dead, ClientPid } ->
            NewNodes = lists:filter(fun(Node) -> Node /= ClientPid end, Nodes),
            loop(Server, NewNodes, server_supply(Server, NewNodes, Version));

        { node_active, ClientPid } ->
			NewNodes = Nodes ++ [ClientPid],
            loop(Server, NewNodes, server_supply(Server, NewNodes, Version))
    end.
    

server_supply(Server, Nodes, Version) ->
    Server ! { supply_nodes, Nodes, Version },
    Version + 1.
