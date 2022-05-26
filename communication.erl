-module(communication).

-include_lib("headers/records.hrl").

-export([multicast/2]).

multicast([], _) -> ok;
multicast([H | T], Data) ->

    Msg = Data#message {
        sender = self()
    },

    H ! { packet, Msg },

	multicast(T, Msg).
