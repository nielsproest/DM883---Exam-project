-module(group).

-include_lib("headers/records.hrl").

-import_all(node).

-export([create/0, create/1, join/1, stream/2]).

create() -> create(2).
create(Capacity) ->
    S = #state{
        source = self(),
        data = [],
        parent = ok,
        children = [],
        capacity = Capacity
    },

    node:run(S).

join(Streamer, Current) ->
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
            node:run(S);
		{ join_refuse, NewCurrent } -> join(Streamer, NewCurrent)
	end.

join(Streamer) -> join(Streamer, Streamer).


stream(_, []) -> ok;
stream(Streamer, [ H | T]) -> 
    Streamer ! { packet, #message {
        data = H,
        sender = self(),
        timestamp = 0,
        stream = 0
    }},
    stream(Streamer, T).