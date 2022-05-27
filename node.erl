-module(node).

-include_lib("headers/records.hrl").

-export([run/3]).


-import_all(rand).
-import_all(lists).
-import_all(network).
-import_all(communication).
-import_all(utility).

% Running nodes continously wait for new packets to handle.
run(S, Callback, Wait) ->
	loop(S, Callback, Wait).

loop(_S, Callback, Timeout) ->
	case rand:uniform() < 0.05 of % 10%
		true -> 
			S = disconnect(_S);
		false -> 
			S = _S
	end,

	receive 
		% Server is notified when a node is dead
		{ node_dead, ClientPid } when S#state.source == self() ->
			NewState = S#state {
				nodes = lists:filter(fun(X) -> X /= ClientPid end, S#state.nodes)
			},
			loop(NewState, Callback, Timeout);

		% Server receives new node
		% Server sends said node a timestamp and capacity
		{ setup, ClientPid, Capacity } when S#state.source == self() -> 
			ClientPid ! { current_state, S#state.timestamp, utility:n_random(Capacity, S#state.nodes) },

			NewState = S#state {
				nodes = S#state.nodes ++ [ClientPid]
			},

			loop(NewState, Callback, Timeout);

		% Server sends new neighbours to client
		{ supply_neighbours, ClientPid, Capacity } when S#state.source == self() ->
			ClientPid ! { receive_neighbours, utility:n_random(Capacity, S#state.nodes) },
			loop(S, Callback, Timeout);

		% Client receives new neighbours from server
		{ receive_neighbours, ClientPids } ->
			NewState = S#state {
				% Filter by unique
				neighbours = lists:usort(S#state.neighbours ++ ClientPids)
			},
			loop(NewState, Callback, Timeout);

		% Adds neighbours to node
		{ join_new, ClientPid } ->
			NewState = S#state {
				neighbours = lists:usort(S#state.neighbours ++ [ClientPid] )
			},

			loop(NewState, Callback, Timeout);

		% Receives data
		{ packet, Data } when length(S#state.backflow) > 10 ->
			Times = backflow_add(S#state.backflow,Data),
			[Callback(X#message.data) || X <- Times],
			Last = utility:lists_last(Times),

			NewState = S#state {
				backflow = [],
				timestamp = Last#message.timestamp
			},
            loop(NewState, Callback, Timeout);

		{ packet, Data } when Data#message.timestamp > (S#state.timestamp + 1) ->
			NewState = S#state {
				backflow = backflow_add(S#state.backflow,Data)
			},

            loop(NewState, Callback, Timeout);
		{ packet, Data } when Data#message.timestamp > S#state.timestamp ->
			% Internal processing
			Times = backflow_add(S#state.backflow,Data),
			[Callback(X#message.data) || X <- Times],
			Last = utility:lists_last(Times),

			communication:multicast(
				S#state.neighbours, Data
			),

			NewState = S#state {
				backflow = [],
				timestamp = Last#message.timestamp
			},

            % Listen for next message and increment timestamp
            loop(NewState, Callback, Timeout)
	after Timeout -> loop(reconnect(S), Callback, Timeout)

    end.

backflow_add(Records, Data) ->
	_Records = Records ++ [Data],
	IDS = lists:usort([X#message.timestamp || X <- _Records]),
	[utility:lists_first([Y || Y <- _Records, Y#message.timestamp == X]) || X <- IDS].

disconnect_aux(_,_, false) -> false;
disconnect_aux(S, X, true) ->
	S#state.source ! { node_dead, X },
	true.
disconnect(S) ->
	NewState = S#state{
		neighbours = lists:filter(fun(X) -> disconnect_aux(S, X, is_process_alive(X)) end, S#state.neighbours)
	},
	if 
		length(S#state.neighbours) > S#state.capacity ->
			ok;
		length(NewState#state.neighbours) /= S#state.neighbours -> 
			S#state.source ! { supply_neighbours, self(), S#state.capacity - length(S#state.neighbours) };
		true -> 
			ok
	end,
	NewState.

reconnect(S) when length(S#state.neighbours) < S#state.capacity -> 
	io:format("Reconnect... \n"),
	self() ! { join_refuse, self() },
	S;
reconnect(S) -> S.