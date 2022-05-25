-module(app).
-export([start/0]).

-include_lib("headers/records.hrl").

-import_all(helpers).
-import_all(server).
-import_all(node).

start() ->

    Server = spawn(fun() -> client:handle() end),
    Intermidiate = spawn(fun() -> client:handle() end),
    Client = spawn(fun() -> client:handle() end),

    Server ! {setup, [Intermidiate, Client]},
    Intermidiate ! {setup, [Server, Client]}, 
    Client ! {setup, [Server, Intermidiate]} ,

    stream(Server, "Hello", 0),

    ok.

stream(_, [], _) -> ok;
stream(Node, [ H | T], N) -> 
    Node ! { packet, #message {
        data = H,
        sender = self(),
        timestamp = N,
        stream = 0
    }},
    stream(Node, T, N + 1).



bootstrap_clients(0, Clients) -> Clients;
bootstrap_clients(N, Clients) ->

    Client = spawn(fun() -> client:handle() end),
    bootstrap_clients(N - 1, Clients ++ [Client]).
