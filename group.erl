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
		nodes = [self()],
        capacity = Capacity
    },

    node:run(S, fun( _Data ) -> ok end, 1000).

% Create client and ask to join
join(Streamer, Callback, Capacity) ->
    Streamer ! { setup, self() },
	receive
		{ current_state, Timestamp, Nodes } -> 

            S = #state {
                source = Streamer,
				timestamp = Timestamp,
				distribution = [],
                neighbours = [],
				nodes = Nodes
            },

            node:run(S, Callback, 0)
	end.

join(Streamer, Callback) -> join(Streamer, Callback, 2).

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



stream(_, [], _) -> ok;
stream(Streamer, [ H | T], N) -> 
    
    Streamer ! { packet, #message {
        data = H,
        sender = self(),
        timestamp = N,
        stream = 0
    }},
    timer:sleep(100),
    stream(Streamer, T, N + 1).

stream(Streamer, Data) -> stream(Streamer, Data, 1). 