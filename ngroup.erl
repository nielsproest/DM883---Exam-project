-module(ngroup).

-include_lib("headers/records.hrl").

-import_all(node).
-import_all(lists).
-import_all(rand).
-import_all(util).

-export([create/1, join/2, stream/3]).

% Create server
create(S, StreamId) ->
	Coordinator = spawn_link( fun() -> coordinator:handle(self(), 1, [self()], 0) end),
    NewState = #state{
        source = maps:put(StreamId, Coordinator, S#state.source),
		timestamp = maps:put(StreamId, 0, S#state.timestamp),
		version = maps:put(StreamId, 0, S#state.version),
		backflow = maps:put(StreamId, [], S#state.backflow),
        neighbours = maps:put(StreamId, [], S#state.neighbours),
		nodes = maps:put(StreamId, [self()], S#state.nodes),
		capacity = 3,
		max_capacity = 5

    },

    node:run(NewState, fun( _Data, _Stream ) -> ok end),

	NewState.

create(StreamId) -> create(#state {
	source = #{},
	timestamp = #{},
	version = #{},
	backflow = #{},
	neighbours = #{},
	nodes = #{},
	capacity = 3,
	max_capacity = 5
}, StreamId).


% Create client and ask to join
join(Streams, Callback) -> 
	S = #state {
		source = #{},
		timestamp = #{},
		version = #{},
		backflow = #{},
		neighbours = #{},
		nodes = #{},
		capacity = 3,
		max_capacity = 5
	},
	NewState = join_all(S, Streams),

	node:run(NewState, Callback).

join_all(S, [] ) -> S;
join_all(S, [ H | T] ) ->
	{ Node, StreamId } = H,

	Node ! { setup, self(), StreamId },

	receive
		{ current_state, Server_S, Stream } -> 
			NewState = S#state {
				source = maps:put(Stream, maps:get(Stream, Server_S#state.source), S#state.source),
				timestamp = maps:put(Stream, maps:get(Stream, Server_S#state.timestamp), S#state.timestamp),	
				backflow = maps:put(Stream, [], S#state.backflow),
				version = maps:put(Stream, maps:get(Stream, Server_S#state.version), S#state.version),
				neighbours = maps:put(Stream, util:n_random(3, maps:get(Stream, Server_S#state.nodes)), S#state.neighbours),
				nodes = maps:put(Stream, maps:get(Stream, Server_S#state.nodes), S#state.nodes),
				capacity = 3,
				max_capacity = 5
			},

			util:send_msg(maps:get(Stream, NewState#state.neighbours), { join_new, self(),  Stream}),

			join_all(NewState, T)
	end.



stream(_, StreamId, [], _) -> ok;
stream(Streamer, StreamId, [ H | T], N) -> 
    
    Streamer ! { packet, #message {
        data = H,
        timestamp = N,
        stream = StreamId
    }},
    timer:sleep(100),
    stream(Streamer, StreamId, T, N + 1).

stream(Streamer, StreamId, Data) -> stream(Streamer, StreamId, Data, 1). 