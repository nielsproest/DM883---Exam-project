-module(app).
-export([start/0]).

-import_all(server).
-import_all(client).

start() ->
    Clients = bootstrap_clients(3, []),

    spawn(fun() -> server:handle(Clients) end),

    ok.


bootstrap_clients(0, Clients) -> Clients;
bootstrap_clients(N, Clients) ->

    Client = spawn(fun() -> client:handle() end),
    bootstrap_clients(N - 1, Clients ++ [Client]).
