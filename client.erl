-module(client).

-include_lib("headers/records.hrl").

-export([handle/0]).

% When node 
handle() -> 
    State = setup(),
    run(State).

% Node awaits a setup message to build the node state
setup() ->
    receive
        { setup, Neighbours } ->
            State = #state{
                neighbours = Neighbours,
                timestamp = 0
            }
    end,
    State.

% Running nodes continously wait for new packets to handle.
run(S) ->
    receive 
        { packet, Data } when Data#message.timestamp >= S#state.timestamp ->
            
            % Internal processing
            io:format("~p received ~p\n", [self(), Data#message.data]),

            % Distribute to some neighbours
            distribute(
                S#state.neighbours, Data
            ),

            % Listen for next message and increment timestamp
            run(S#state {
                timestamp = S#state.timestamp + 1
            })

    end.

distribute([], _) -> ok;
distribute([H | T], Data) ->

    Msg = Data#message {
        sender = self()
    },

    H ! { packet, Msg },

	distribute(T, Msg).
