-module(communication).

-include_lib("headers/records.hrl").

-export([multicast/2]).

multicast(Neighbours, Data) ->
	send(Neighbours, Data).

send([], _) -> ok;
send([H | T], Data) ->

    H ! { packet, Data },

    send(T, Data).
