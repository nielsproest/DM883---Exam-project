-module(communication).

-include_lib("headers/records.hrl").

-export([multicast/3]).

multicast(Neighbours, Data, Distribution) ->

    Timestamp = Data#message.timestamp,

    case lists:member(Timestamp, Distribution) of
        true -> Distribution;
        false -> send(Neighbours, Data), Distribution ++ [Timestamp]
    end.
    

send([], _) -> ok;
send([H | T], Data) ->

    Msg = Data#message {
        sender = self()
    },

    H ! { packet, Msg },

    send(T, Msg).
