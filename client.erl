-module(client).
-export([handle/1]).

-record(state, {
	neighbours :: [],
    timestamp :: integer()
}).

-record(message, {
	data :: byte(),
    sender :: pid(),
    timestamp :: integer(),
    stream :: integer()

}).

handle(Neighbours) -> 

    S = #state{
        neighbours = Neighbours,
        timestamp = 0
    },

    receive 
        { packet, Data, Timestamp, Sender, Stream } when Timestamp > S#state.timestamp ->
            
            send_to

        _ -> ok
    end.
        

sent_to_neighbours([]) ->
sent_to_neighbours([ H | T]) ->
    H ! {}
