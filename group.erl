-module(group).

-include_lib("headers/records.hrl").

-import_all(node).

-export([create/0, create/1, join/1, join/2, stream/2]).

create() -> create(2).
create(Capacity) ->
    S = #state{
        source = self(),
        data = [],
        parent = ok,
        children = [],
        capacity = Capacity
    },

    node:run( S, fun( _Data ) -> ok end ).

join(Streamer, Current, Callback) ->
    Current ! { join, self() },
	receive
		{ join_ok, ParentState } -> 
            S = #state {
                source = Streamer,
                data = [],
                parent = Current,
                children = [],
                capacity = ParentState#state.capacity
            },
            node:run(S, Callback);
		{ join_refuse, NewCurrent } -> join(Streamer, NewCurrent, Callback)
	end.


join(Streamer, Callback) -> join(Streamer, Streamer, Callback).
join(Streamer) -> join(Streamer, fun(_Data) -> ok end).

stream(_, []) -> ok;
stream(Streamer, [ H | T]) -> 
    Streamer ! { packet, #message {
        data = H,
        sender = self(),
        timestamp = 0,
        stream = 0
    }},
    stream(Streamer, T).