-module(ngroup).

-include_lib("headers/records.hrl").

-import_all(node).
-import_all(lists).
-import_all(rand).
-import_all(utility).

-export([create/0, create/1, join/2, join/3, stream/2]).

% Create server
create() -> create(3).
create(Capacity) ->
    S = #state{
        source = self(),
		timestamp = 0,
		version = 0,
		backflow = [],
        neighbours = [],
		nodes = [self()],
        capacity = Capacity
    },

    node:run(S, fun( _Data ) -> ok end, 1000).

% Create client and ask to join
join(Streamer, Callback, Capacity) ->
	Streamer ! { setup, self() },
	receive
		{ current_state, Server_S } -> 
			S = #state {
				source = Streamer,
				timestamp = Server_S#state.timestamp,
				backflow = [],
				version = Server_S#state.version,
                neighbours = utility:n_random(Capacity, Server_S#state.nodes),
				capacity = Capacity,
				nodes = Server_S#state.nodes
			},
			utility:send_msg(S#state.neighbours, { join_new, self() }),
			node:run(S, Callback, 1500)
	end.
join(Streamer, Callback) -> join(Streamer, Callback, 5).



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