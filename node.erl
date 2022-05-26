-module(node).

-include_lib("headers/records.hrl").

-export([run/2]).


-import_all(rand).
-import_all(lists).
-import_all(join).



% Running nodes continously wait for new packets to handle.
run(S, Callback) ->
    receive 
		{ join, ClientPid } -> 

			NewState = join:attempt(
				S, ClientPid
			),

			run(NewState, Callback);

        { packet, Data } ->
            
            % Internal processing
            Callback(Data#message.data),

            % Distribute to some neighbours
            communication:multicast(
                S#state.children, Data
            ),

            % Listen for next message and increment timestamp
            run(S, Callback)
    end.
