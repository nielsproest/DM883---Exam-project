-module(node3).

-include_lib("headers/records.hrl").

-export([server_setup/0]).
-export([client_setup/2]).

-import_all(rand).
-import_all(lists).

% Node awaits a setup message to build the node state
server_setup() ->
    run(#state{
                source = ok,
				data = [],
                parent = ok,
				children = []
            }
	).

client_setup(S,P) ->
	P ! { join, self() },
	receive
		{ join_ok } ->
			run(#state {
				source = S,
				data = [],
				parent = P,
				children = []
			});
		{ join_refuse, New_p } ->
			client_setup(S,New_p)
	end.

% Running nodes continously wait for new packets to handle.
run(S) ->
    receive 
		{ join, Pid } when length(S#state.children) =< S#state.capacity -> 
			Pid ! { join_ok },
			run(S#state {
				children = S#state.children ++ [Pid]
			});

		{ join, Pid } ->
			Pid ! { join_refuse, lists:nth(rand:uniform(length(S#state.children)), S#state.children) },
			run(S);

        { packet, Data } when Data#message.timestamp >= S#state.timestamp ->
            % Internal processing
            io:format("~p received ~c\n", [self(), Data#message.data]),

            % Distribute to some neighbours
            distribute(
                S#state.children, Data
            ),

            % Listen for next message and increment timestamp
            run(S#state {
                timestamp = Data#message.timestamp
            })
    end.

distribute([], _) -> ok;
distribute([H | T], Data) ->

    Msg = Data#message {
        sender = self()
    },

    H ! { packet, Msg },

	distribute(T, Msg).
