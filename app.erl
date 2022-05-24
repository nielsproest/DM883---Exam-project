-module(app).
-export([start/0]).

-include_lib("headers/records.hrl").

-import_all(helpers).
-import_all(server).
-import_all(client).

start() ->

    Server = spawn(fun() -> client:handle() end),
    Intermidiate = spawn(fun() -> client:handle() end),
    Client = spawn(fun() -> client:handle() end),

    Server ! {setup, [Intermidiate, Client]},
    Intermidiate ! {setup, [Server, Client]}, 
    Client ! {setup, [Server, Intermidiate]} ,

    stream(Server, 0),

    ok.

stream(_, 10) -> ok;
stream(Node, N) -> 
    Node ! { packet, #message {
        data = helpers:generate_data(),
        sender = self(),
        timestamp = N,
        stream = 0
    }},
    stream(Node, N + 1).



bootstrap_clients(0, Clients) -> Clients;
bootstrap_clients(N, Clients) ->

    Client = spawn(fun() -> client:handle() end),
    bootstrap_clients(N - 1, Clients ++ [Client]).
