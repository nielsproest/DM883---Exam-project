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
		backflow = [],
        neighbours = [],
		nodes = [self()],
        capacity = Capacity
    },

    node:run(S, fun( _Data ) -> ok end, 1000).

% Create client and ask to join
join(Streamer, Callback, Capacity) ->
	Streamer ! { setup, self(), Capacity },
	receive
		{ current_state, Timestamp, Neighbours } -> 
			S = #state {
				source = Streamer,
				timestamp = Timestamp,
				backflow = [],
                neighbours = Neighbours,
				capacity = Capacity,
				nodes = []
			},
			utility:send_msg(Neighbours, { join_new, self() }),
			node:run(S, Callback, 1500)
	end.
join(Streamer, Callback) -> join(Streamer, Callback, 2).



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