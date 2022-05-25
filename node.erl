-module(node).

-include_lib("headers/records.hrl").

-export([run/1]).


-import_all(rand).
-import_all(lists).



% Running nodes continously wait for new packets to handle.
run(S) ->
    receive 
		{ join, ClientPid } -> 

			run(
				attempt_to_join(S, ClientPid)
			);

        { packet, Data } ->
            
            % Internal processing
            io:format("~p received ~c\n", [self(), Data#message.data]),

            % Distribute to some neighbours
            distribute(
                S#state.children, Data
            ),

            % Listen for next message and increment timestamp
            run(S)
    end.

attempt_to_join(S, Pid) ->
	case length(S#state.children) =< S#state.capacity  of
		true -> 
			accept_join(S, Pid);
		false -> 
			refuse_join(S, Pid)
	end.

accept_join(S, Pid) -> 
	Pid ! { join_ok },
	S#state {
		children = S#state.children ++ [Pid]
	}.

refuse_join(S, Pid) ->
	Pid ! { join_refuse, lists:nth(rand:uniform(length(S#state.children)), S#state.children) },
	S.


distribute([], _) -> ok;
distribute([H | T], Data) ->

    Msg = Data#message {
        sender = self()
    },

    H ! { packet, Msg },

	distribute(T, Msg).
