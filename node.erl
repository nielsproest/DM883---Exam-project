-module(node).

-include_lib("headers/records.hrl").

-export([run/3]).


-import_all(rand).
-import_all(lists).
-import_all(network).
-import_all(communication).


% Running nodes continously wait for new packets to handle.
run(S, Callback, Wait) ->
	loop(S, Callback, Wait).


loop(S, Callback, Timeout) ->
    receive 
		{ setup, ClientPid } when S#state.source == self() -> 
			ClientPid ! { current_state, S#state.timestamp, S#state.nodes },
			NewState = S#state {
				nodes = S#state.nodes ++ [ClientPid]
			},

			loop(NewState, Callback, Timeout);

		{ join, Pid } ->
			NewState = S#state {
				nodes = S#state.nodes ++ [Pid],
				neighbours = S#state.neighbours ++ [Pid]
			},

			loop(NewState, Callback, Timeout);

        { packet, Data } ->
            

            % Internal processing
			Timestamp = case (S#state.timestamp < Data#message.timestamp) of
				true -> Callback(Data#message.data), Data#message.timestamp;
				false -> S#state.timestamp
			end,

			Distribution = communication:multicast(
				S#state.neighbours, Data, S#state.distribution
			),

			NewState = S#state {
				distribution = Distribution,
				timestamp = Timestamp
			},

            % Listen for next message and increment timestamp
            loop(NewState, Callback, 1500)
	after 1500 -> reconnect(S#state.nodes), loop(S, Callback, 0), ok

    end.


reconnect(Nodes) -> 
	io:format("Reconnect... \n"),
	RandomPid = lists:nth(rand:uniform(length(Nodes)), Nodes),
	RandomPid ! { join, self() }.