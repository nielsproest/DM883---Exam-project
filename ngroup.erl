-module(ngroup).

-include_lib("headers/records.hrl").

-import_all(node).
-import_all(lists).
-import_all(rand).
-import_all(util).

-export([create/0, create/1, join/2, join/3, stream/2]).

% Create server
create(S) ->
	Coordinator = spawn_link( fun() -> coordinator:handle(self(), 1, [self()], 0) end),
    NewState = #state{
        source = maps:put(1, Coordinator, S#state.source),
		timestamp = maps:put(1, 0, S#state.timestamp),
		version = maps:put(1, 0, S#state.version),
		backflow = maps:put(1, [], S#state.backflow),
        neighbours = maps:put(1, [], S#state.neighbours),
		nodes = maps:put(1, [self()], S#state.nodes)
    },

    node:run(NewState, fun( _Data ) -> ok end),

	NewState.
create() -> create(#state {
	source = #{},
	timestamp = #{},
	version = #{},
	backflow = #{},
	neighbours = #{},
	nodes = #{},
	capacity = 3,
	max_capacity = 5
}).


% Create client and ask to join
join(Streamer, Callback, Capacity) -> join(Streamer, #state {
	source = #{},
	timestamp = #{},
	version = #{},
	backflow = #{},
	neighbours = #{},
	nodes = #{},
	capacity = 3,
	max_capacity = 5
}, Callback, Capacity).
join(Streamer, Callback) -> join(Streamer, Callback, 5).
join(Streamer, S, Callback, Capacity) ->
	Streamer ! { setup, self(), 1 },
	receive
		{ current_state, Server_S, Stream } -> 
			io:format("Dump ~p\n", [Stream]),
			NewState = S#state {
				source = maps:put(Stream, maps:get(Stream, Server_S#state.source), S#state.source),
				timestamp = maps:put(Stream, maps:get(Stream, Server_S#state.timestamp), S#state.timestamp),	
				backflow = maps:put(Stream, [], S#state.backflow),
				version = maps:put(Stream, maps:get(Stream, Server_S#state.version), S#state.version),
                neighbours = maps:put(Stream, util:n_random(Capacity, maps:get(Stream, Server_S#state.neighbours)), S#state.neighbours),
				capacity = Capacity,
				max_capacity = Capacity + 2,
				nodes = maps:put(Stream, maps:get(Stream, Server_S#state.nodes), S#state.nodes)
			},

			% Request neighbours to stream to them
			util:send_msg(S#state.neighbours, { join_new, self(), 1 }),

			io:format("print: ~p \n ", [S#state.neighbours]),

			node:run(NewState, Callback)
	end,

	NewState.


stream(_, [], _) -> ok;
stream(Streamer, [ H | T], N) -> 
    
    Streamer ! { packet, #message {
        data = H,
        timestamp = N,
        stream = 1
    }},
    timer:sleep(100),
    stream(Streamer, T, N + 1).

stream(Streamer, Data) -> stream(Streamer, Data, 1). 