-module(node).

-include_lib("headers/records.hrl").

-export([run/2]).


-import_all(rand).
-import_all(lists).
-import_all(network).
-import_all(communication).
-import_all(util).
-import_all(pinger).


% Running nodes continously wait for new packets to handle.
run(S, Callback) ->
	Myself = self(),
	spawn_link(fun() -> pinger:run(Myself) end),
	loop(S, Callback).

loop(S, Callback) ->

	receive 

		% Server receives new node
		{ setup, Client, Stream }  -> 
			
			Client ! { current_state, S, Stream },

			maps:get(Stream, S#state.source) ! { node_active, Client },

			NewState = S#state {
				nodes = maps:update_with(Stream, fun(V) -> V ++ [Client] end, S#state.nodes)
			},

			loop(NewState, Callback);

		{ update_neighbours } ->

			AllKeys = maps:keys(S#state.source),
			RandomKey = lists:nth(rand:uniform(length(AllKeys)), AllKeys),	

			StreamNeighbours = maps:get(RandomKey, S#state.neighbours),
			StreamNodes = maps:get(RandomKey, S#state.nodes),
			Source = maps:get(RandomKey, S#state.source),

			send_kill_signal(Source, dead_neighbours(StreamNeighbours)),

			AliveNeighbours = alive_neighbours(StreamNeighbours),

			% Check if it is possible to assign new neighbours
			case length(AliveNeighbours) < S#state.capacity of
				true -> 
					Neighbours = assign_new_neighbours(S#state.capacity, AliveNeighbours, StreamNodes),
					util:send_msg(Neighbours, { join_new, self() , RandomKey });
				false -> 
					Neighbours = AliveNeighbours
			end,

			loop(S#state {
				neighbours = maps:update(RandomKey, Neighbours, S#state.neighbours)
			}, Callback);

		% Client receives node list
		{ supply_nodes, Nodes, Stream, Version } ->

			case Version > maps:get(Stream, S#state.version) of
				true -> 
					NewState = S#state {
					nodes = maps:update(Stream, Nodes, S#state.nodes),
					neighbours = maps:update_with(Stream, fun(V) -> lists:filter(fun(X) -> lists:member(X, Nodes) end, V) end,  S#state.neighbours),
					version = maps:update(Stream, Version, S#state.version)
				},
				util:send_msg(S#state.neighbours, {supply_nodes, Nodes, Stream, Version}),
				loop(NewState, Callback);
				false -> loop(S, Callback)
			end;
		{ join_new, Client, Stream } ->

			case length(maps:get(Stream, S#state.neighbours)) =< S#state.capacity of
				true -> 
					NewState = S#state {
						neighbours = maps:update_with(Stream, fun(V) -> lists:usort(V ++ [Client]) end, S#state.neighbours)
					},
		
					loop(NewState, Callback);
				false -> 
					Client ! { join_refuse, self(), Stream },
					loop(S, Callback)
			end;

		% Client refused to join
		{ join_refuse, Client, Stream} ->
			NewState = S#state {
				neighbours = maps:update_with(Stream, fun(V) -> lists:filter(fun(X) -> X /= Client end,  V) end, S#state.neighbours)
			},
			loop(NewState, Callback);
		{ packet, Msg }  ->

	
			Stream = Msg#message.stream,
			Timestamp = maps:get(Stream, S#state.timestamp),
			Backflow = maps:get(Stream, S#state.backflow),

			case Msg#message.timestamp > Timestamp of
				true ->
					case length(Backflow) > 10 of
						true -> loop(S#state {
							backflow = maps:update(Stream, [], S#state.backflow),
							timestamp = maps:update(Stream, Msg#message.timestamp, S#state.timestamp)
						}, Callback);
						false ->
							MessagesSorted = lists:usort(fun(A,B) -> A#message.timestamp =< B#message.timestamp end, Backflow ++ [Msg]),
							Neighbours = maps:get(Stream, S#state.neighbours),

							MessagesToPlay = messages_to_play(MessagesSorted, Timestamp),

							NewBackflow = lists:nthtail(length(MessagesToPlay), MessagesSorted),
							
							% Send out messages
							lists:foreach(fun(Message) -> 
								Callback(Message#message.data, Stream),
								communication:multicast(Neighbours, Message)
							end, MessagesToPlay),

							NewState = S#state {
								backflow = maps:update(Stream, NewBackflow, S#state.backflow),
								timestamp = maps:update_with(Stream, fun(V) -> V + length(MessagesToPlay) end, S#state.timestamp)
							},

							% Listen for next message and increment timestamp
							loop(NewState, Callback)
					end;
				false -> loop(S, Callback)
			end
    end.

messages_to_play(Backflow, CurrentTimestamp) -> messages_to_play(Backflow, CurrentTimestamp, []).


messages_to_play([ H | T ], CurrentTimestamp, Messages) when H#message.timestamp - CurrentTimestamp == 1  ->
	messages_to_play(T, CurrentTimestamp + 1, Messages ++ [ H ]);

messages_to_play(_, _, Messages) -> Messages.

%%----------------------------------------------------------------------------
%% @doc Select a random set of new neighbours from global set and assign
%%	them to the current node.
%% @end
%%----------------------------------------------------------------------------
-spec assign_new_neighbours(integer(), [node()], [node()]) -> [node()].
assign_new_neighbours(Capacity, Neighbours, Nodes) ->
	RemainingCapacity = Capacity - length(Neighbours),
	NewNeighbours = lists:usort(Neighbours ++ util:n_random(RemainingCapacity, Nodes)),
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

