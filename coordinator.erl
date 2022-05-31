-module(coordinator).

-include_lib("headers/records.hrl").

-export([handle/4]).


handle(Server, Stream, Nodes, Version) ->
    loop(Server, Stream, Nodes, Version).


loop(Server, Stream, Nodes, Version) ->
    receive 
        { node_dead, ClientPid } ->
            NewNodes = lists:filter(fun(Node) -> Node /= ClientPid end, Nodes),
            loop(Server, Stream, NewNodes, server_supply(Server, Stream, NewNodes, Version));

        { node_active, ClientPid } ->
			NewNodes = Nodes ++ [ClientPid],
            loop(Server, Stream, NewNodes, server_supply(Server, Stream, NewNodes, Version))
    end.
    

server_supply(Server, Stream, Nodes, Version) ->
    Server ! { supply_nodes, Nodes, Stream, Version },
    Version + 1.
