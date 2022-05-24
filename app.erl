-module(app).
-export([start/0]).

-import_all(server).
-import_all(client).

start() ->

    Server = spawn(fun() -> client:handle() end),
    Intermidiate = spawn(fun() -> client:handle() end),
    Client = spawn(fun() -> client:handle() end),

    Server ! {setup, [Intermidiate, Client]},
    Intermidiate ! {setup, [Server, Client]}, 
    Client ! {setup, [Server, Intermidiate]} ,


    Server ! { stream },

    Client ! { rec },

    ok.


bootstrap_clients(0, Clients) -> Clients;
bootstrap_clients(N, Clients) ->

    Client = spawn(fun() -> client:handle() end),
    bootstrap_clients(N - 1, Clients ++ [Client]).
