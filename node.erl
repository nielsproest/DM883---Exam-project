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
	% Pings neighbours 5% of the time
	case rand:uniform() < 0.05 of
		true -> 
			__S = reconnect(disconnect(_S));
		false -> 
			__S = _S
	end,
	% Server supplies node list 5% of the time
	case rand:uniform() < 0.05 of
		true -> 
			S = server_supply(__S);
		false -> 
			S = __S
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
		{ setup, ClientPid } when S#state.source == self() -> 
			ClientPid ! { current_state, S },

			NewState = S#state {
				nodes = S#state.nodes ++ [ClientPid]
			},

			loop(NewState, Callback, Timeout);

		% Client receives node list
		{ supply_nodes, Nodes, Version } when Version > S#state.version ->
			NewState = S#state {
				nodes = Nodes,
				neighbours = lists:filter(fun(X) -> lists:member(X, Nodes) end, S#state.neighbours),
				version = Version
			},
			utility:send_msg(S#state.neighbours, {supply_nodes, Nodes, Version}),

			loop(NewState, Callback, Timeout);

		% Adds neighbours to node
		{ join_new, ClientPid } when length(S#state.neighbours) =< S#state.capacity ->
			NewState = S#state {
				neighbours = lists:usort(S#state.neighbours ++ [ClientPid] )
			},

			loop(NewState, Callback, Timeout);

		{ join_new, ClientPid } ->
			ClientPid ! { join_refuse, self() },
			loop(S, Callback, Timeout);

		% Client refused to join
		{ join_refuse, ClientPid } ->
			NewState = S#state {
				neighbours = lists:filter(fun(X) -> X /= ClientPid end, S#state.neighbours)
			},
			loop(NewState, Callback, Timeout);

		% Receives data
		{ packet, Data } when length(S#state.backflow) > 10 ->
			PrevState = backflow_add(S,Data,Callback),
			[Callback(X#message.data) 
				|| X <- PrevState#state.backflow],
			[communication:multicast(_S#state.neighbours, X) 
				|| X <- PrevState#state.backflow],

			NewState = PrevState#state {
				backflow = [],
				timestamp = Data#message.timestamp
			},
            loop(NewState, Callback, Timeout);

		{ packet, Data } when Data#message.timestamp > S#state.timestamp ->
			% Internal processing
			NewState = backflow_add(S,Data,Callback),

            % Listen for next message and increment timestamp
            loop(NewState, Callback, Timeout)
	after Timeout -> loop(S, Callback, Timeout)

    end.

% Server supply's new node list
server_supply(S) when S#state.source == self() ->
	NewState = S#state {
		version = S#state.version + 1
	},
	utility:send_msg(S#state.neighbours, { supply_nodes, NewState#state.nodes, NewState#state.version }),
	NewState;
server_supply(S) -> S.

% Handles "backflows" (missing messages)
backflow_process(S, [H | T], C, A, B) when B-A == 1 ->
	% Send message along
	communication:multicast(
		S#state.neighbours, H
	),
	C(H#message.data),
	% Update timestamp
	NewState = S#state {
		timestamp = H#message.timestamp
	},
	backflow_process(NewState,T,C,A,H#message.timestamp);
backflow_process(S, T, _, _, _) ->
	S#state {
		backflow = T
	}.
backflow_add(S, Data, C) ->
	% Current records
	_Records = S#state.backflow ++ [Data],
	% Their timestamps, sorted
	IDS = lists:usort([X#message.timestamp || X <- _Records]),
	% Records, sorted
	Datas = [utility:lists_first([Y || Y <- _Records, Y#message.timestamp == X]) || X <- IDS],
	First = utility:lists_first(Datas),
	backflow_process(S, Datas, C, S#state.timestamp, First#message.timestamp).

% Pings neighbours
disconnect_aux(S, X, false) ->
	S#state.source ! { node_dead, X },
	false;
disconnect_aux(_,_, true) -> true.
disconnect(S) ->
	S#state{
		neighbours = lists:filter(fun(X) -> disconnect_aux(S, X, is_process_alive(X)) end, S#state.neighbours)
	}.

% Reconnect missing nodes
reconnect(S) when length(S#state.neighbours) < S#state.capacity -> 
	io:format("Reconnect... \n"),
	
	Missing = S#state.capacity - length(S#state.neighbours),
	NewNeighbours = utility:n_random(Missing, S#state.nodes),
	NewState = S#state {
		neighbours = lists:usort(S#state.neighbours ++ utility:n_random(Missing, S#state.nodes))
	},
	utility:send_msg(NewNeighbours, { join_new, self() }),
	NewState;
reconnect(S) -> S.
