-module(client).
-export([handle/0]).

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

packet_handle(S, Data) ->
	io:format("~p received ~p\n", [self(), Data#message.data]),
    sent_to_neighbours(
		lists:filter(
			fun({Pid}) -> Pid /= Data#message.sender end, 
			S#state.neighbours
		), 
		{ packet, Data#message {
			sender = self()
		} }
	),
	client_loop(S#state {
		timestamp = Data#message.timestamp
	}).

send_handle(S, Msg) ->
	io:format("~p sends ~p\n", [self(), Msg]),
	sent_to_neighbours(
		S#state.neighbours, 
		{ packet, #message {
			data = Msg,
			sender = self(),
			timestamp = S#state.timestamp,
			stream = 0
		} }
	),
	client_loop(S#state {
		timestamp = S#state.timestamp + 1
	}).

client_loop(S) ->
    receive 
		{ setup, Neighbours } ->
			client_loop(#state{
		        neighbours = Neighbours,
		        timestamp = 0
			});
        { packet, Data } when Data#message.timestamp >= S#state.timestamp ->
			packet_handle(S,Data);
		{ send, Data } -> 
			send_handle(S, Data);

        _ -> client_loop(S)
    end.
 
handle() -> client_loop(#state{}).

sent_to_neighbours([], _) -> ok;
sent_to_neighbours([H | T], Msg) ->
    H ! Msg,
	sent_to_neighbours(T, Msg).
