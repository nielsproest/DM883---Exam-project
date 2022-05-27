-module(communication).

-include_lib("headers/records.hrl").

-export([multicast/2]).

multicast(Neighbours, Data) ->
	send(Neighbours, Data).

send([], _) -> ok;
send([H | T], Data) ->

    Msg = Data#message {
        sender = self()
    },

    H ! { packet, Msg },

    send(T, Msg).
