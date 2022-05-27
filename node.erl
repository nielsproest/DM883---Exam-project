-module(node).

-include_lib("headers/records.hrl").

-export([run/1, run/2, run/3]).


-import_all(rand).
-import_all(lists).
-import_all(network).
-import_all(communication).
-import_all(buffer).



% Running nodes continously wait for new packets to handle.
run(S, Callback, Wait) ->

	Buffer = spawn_link( fun() -> buffer:run([]) end),

	NewState = S#state {
		buffer = Buffer
	},

	loop(NewState, Callback, Wait).

run(S, Wait) ->
	run(S, fun() -> ok end, Wait).

run(S) ->
	run(S, fun() -> ok end, 0).


loop(S, Callback, Timeout) ->
    receive 
		{ join, ClientPid } -> 

			NewState = network:join(
				S, ClientPid
			),

			loop(NewState, Callback, Timeout);

        { packet, Data } ->
            
            % Internal processing
            Callback(Data#message.data),

			spawn_link( fun() ->  
					timer:sleep(2500),
					communication:multicast(
						S#state.children, Data
					)
				end
			),


            % Listen for next message and increment timestamp
            loop(S, Callback, 1500)
	after Timeout -> reconnect(), ok

    end.


reconnect() -> io:format("Reconnect.").