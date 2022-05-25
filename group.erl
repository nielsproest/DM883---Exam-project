-module(group).

-include_lib("headers/records.hrl").

-import_all(node).

-export([create/0, join/1]).

create() ->
    S = #state{
        source = self(),
        data = [],
        parent = ok,
        children = []
    },

    node:run(S).

join(Streamer, Current) ->
    Current ! { join, self() },

    S = #state {
        source = Streamer,
        data = [],
        parent = Current,
        children = []
    },

	receive
		{ join_ok } -> node:run(S);
		{ join_refuse, NewCurrent } -> join(Streamer, NewCurrent)
	end.

join(Streamer) -> join(Streamer, Streamer).