-module(node).

-include_lib("headers/records.hrl").

-export([run/1, run/2, run/3]).


-import_all(rand).
-import_all(lists).
-import_all(network).
-import_all(communication).


% Running nodes continously wait for new packets to handle.
run(S, Callback, Wait) ->
	loop(S, Callback, Wait).

run(S, Wait) ->
	run(S, fun() -> ok end, Wait).

run(S) ->
	run(S, fun() -> ok end, 0).


loop(S, Callback, Timeout) ->
    receive 
		{ join_ask, ClientPid, Capacity } when S#state.source == self() -> 

			NewState = group:join_ok(
				S, ClientPid, Capacity
			),

			loop(NewState, Callback, Timeout);

        { packet, Data } ->
            
            % Internal processing
            Callback(Data#message.data),

			spawn_link( fun() ->  

					communication:multicast(
						S#state.neighbours, Data, S#state.distribution
					)
				end
			),


            % Listen for next message and increment timestamp
            loop(S, Callback, 1500)
	after Timeout -> reconnect(), ok

    end.


reconnect() -> io:format("Reconnect.").