-module(node).

-include_lib("headers/records.hrl").

-export([run/2]).


-import_all(rand).
-import_all(lists).
-import_all(network).
-import_all(communication).
-import_all(utility).
-import_all(pinger).


% Running nodes continously wait for new packets to handle.
run(S, Callback) ->
	Myself = self(),
	spawn_link(fun() -> pinger:run(Myself) end),
	loop(S, Callback).

loop(S, Callback) ->

	receive 

		% Server receives new node
		{ setup, ClientPid }  -> 
			ClientPid ! { current_state, S },

			S#state.source ! { node_active, ClientPid },


			NewState = S#state {
				nodes = S#state.nodes ++ [ClientPid]
			},

			loop(NewState, Callback);

		{ update_neighbours } ->
	
			send_kill_signal(S#state.source, dead_neighbours(S#state.neighbours)),

			AliveNeighbours = alive_neighbours(S#state.neighbours),

			% Check if it is possible to assign new neighbours
			case length(AliveNeighbours) < S#state.capacity of
				true -> 
					Neighbours = assign_new_neighbours(S#state.capacity, AliveNeighbours, S#state.nodes);
				false -> 
					Neighbours = AliveNeighbours
			end,

			loop(S#state {
				neighbours = Neighbours
			}, Callback);

		% Client receives node list
		{ supply_nodes, Nodes, Version } when Version > S#state.version ->
			NewState = S#state {
				nodes = Nodes,
				neighbours = lists:filter(fun(X) -> lists:member(X, Nodes) end, S#state.neighbours),
				version = Version
			},
			utility:send_msg(S#state.neighbours, {supply_nodes, Nodes, Version}),

			loop(NewState, Callback);

		{ join_new, ClientPid } ->

			case length(S#state.neighbours) =< S#state.capacity of
				true -> 
					NewState = S#state {
						neighbours = lists:usort(S#state.neighbours ++ [ClientPid] )
					},
		
					loop(NewState, Callback);
				false -> 
					ClientPid ! { join_refuse, self() },
					loop(S, Callback)
			end;

		% Client refused to join
		{ join_refuse, ClientPid } ->
			NewState = S#state {
				neighbours = lists:filter(fun(X) -> X /= ClientPid end, S#state.neighbours)
			},
			loop(NewState, Callback);

		% Receives data
		{ packet, Data } when length(S#state.backflow) > 10 ->
			PrevState = backflow_add(S,Data,Callback),
			[Callback(X#message.data) || X <- PrevState#state.backflow],
			[communication:multicast(S#state.neighbours, X) || X <- PrevState#state.backflow],

			NewState = PrevState#state {
				backflow = [],
				timestamp = Data#message.timestamp
			},
            loop(NewState, Callback);

		{ packet, Data } when Data#message.timestamp > S#state.timestamp ->
			% Internal processing
			NewState = backflow_add(S,Data,Callback),

            % Listen for next message and increment timestamp
            loop(NewState, Callback)
    end.


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

%%----------------------------------------------------------------------------
%% @doc Select a random set of new neighbours from global set and assign
%%	them to the current node.
%% @end
%%----------------------------------------------------------------------------
-spec assign_new_neighbours(integer(), [node()], [node()]) -> [node()].
assign_new_neighbours(Capacity, Neighbours, Nodes) ->
	Missing = Capacity - length(Neighbours),
	NewNeighbours = lists:usort(Neighbours ++ utility:n_random(Missing, Nodes)),
	utility:send_msg(NewNeighbours, { join_new, self() }),
	NewNeighbours.



%%----------------------------------------------------------------------------
%% @doc Send a kill signal for a list of nodes to a source.
%% @end
%%----------------------------------------------------------------------------
-spec send_kill_signal(node(), [node()]) -> ok.
send_kill_signal(_, []) -> ok;
send_kill_signal(Source, [ H | T]) -> Source ! { node_dead, H }, send_kill_signal(Source, T).

%%----------------------------------------------------------------------------
%% @doc Compute the subset of alive neighbours.
%% @end
%%----------------------------------------------------------------------------
-spec alive_neighbours([node()]) -> [node()].
alive_neighbours(Neighbours) -> lists:filter(fun(X) -> is_process_alive(X) end, Neighbours).

%%----------------------------------------------------------------------------
%% @doc Compute the subset of dead neighbours.
%% @end
%%----------------------------------------------------------------------------
-spec dead_neighbours([node()]) -> [node()].
dead_neighbours(Neighbours) -> lists:filter(fun(X) -> not is_process_alive(X) end, Neighbours).

