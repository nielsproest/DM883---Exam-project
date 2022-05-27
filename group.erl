-module(group).

-include_lib("headers/records.hrl").

-import_all(node).
-import_all(lists).
-import_all(rand).

-export([create/0, create/1, join/2, join/3, join_ok/3, stream/2]).

% Create server
create() -> create(3).
create(Capacity) ->
    S = #state{
        source = self(),
		timestamp = 0,
		distribution = [],
        neighbours = [],
		nodes = [],
        capacity = Capacity
    },

    node:run(S, fun( _Data ) -> ok end).

% Create client and ask to join
join(Streamer, Callback, Capacity) ->
    Streamer ! { join_ask, self(), Capacity },
	receive
		{ join_ok, Timestamp, Nodes } -> 
            S = #state {
                source = Streamer,
				timestamp = Timestamp,
				distribution = [],
                neighbours = n_random(Capacity, Nodes),
				nodes = Nodes,
                capacity = Capacity
            },
            node:run(S, Callback)
	end.

% Server responds to client that wants to join
join_ok(S, Node, Capacity) ->
	Node ! { join_ok, S#state.timestamp, S#state.nodes },
	S#state {
		nodes = S#state.nodes ++ [Node]
	}.

n_random(N, List) -> n_random(N, [], List).
n_random(0, Out, _) ->
	lists:usort(Out);
n_random(N, Out, List) ->
	Random_client = lists:nth(rand:uniform(length(List)), List),
	n_random(N-1, Out ++ [Random_client], List).

join(Streamer, Callback) -> join(Streamer, Callback, 2).

stream(_, []) -> ok;
stream(Streamer, [ H | T]) -> 
    
    Streamer ! { packet, #message {
        data = H,
        sender = self(),
        timestamp = 0,
        stream = 0
    }},
    timer:sleep(1000),
    stream(Streamer, T).