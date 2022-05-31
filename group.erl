-module(group).

-include_lib("headers/records.hrl").

-import_all(node).
-import_all(lists).
-import_all(rand).
-import_all(util).

-export([create/0, create/1, join/2, join/3, stream/2]).

% Create server
create() -> create(3).
create(Capacity) ->
	Coordinator = spawn_link( fun() -> coordinator:handle(self(), 1, [self()], 0) end),
    S = #state{
        source = #{1 => Coordinator },
		timestamp = #{1 => 0},
		version = #{1 => 0},
		backflow = #{1 => []},
        neighbours = #{1 => []},
		nodes = #{1 => self()},
        capacity = Capacity,
		max_capacity = Capacity + 2
    },

    node:run(S, fun( _Data ) -> ok end).

% Create client and ask to join
join(Streamer, Callback, Capacity) ->
	Streamer ! { setup, self() },
	receive
		{ current_state, Server_S, Stream } -> 
			S = #state {
				source = #{ 1 => Server_S#state.source },
				timestamp = #{ 1 => Server_S#state.timestamp },
				backflow = #{ 1 => []},
				version = #{1 => Server_S#state.version},
                neighbours = #{ 1 => util:n_random(Capacity, Server_S#state.nodes) },
				capacity = Capacity,
				max_capacity = Capacity + 2,
				nodes = #{ 1 => Server_S#state.nodes }
			},

			% Request neighbours to stream to them
			util:send_msg(S#state.neighbours, { join_new, self() }),

			io:format("print: ~p \n ", [S#state.neighbours]),

			node:run(S, Callback)
	end.
join(Streamer, Callback) -> join(Streamer, Callback, 5).



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