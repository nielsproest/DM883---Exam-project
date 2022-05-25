-module(app).
-export([start/0]).

-include_lib("headers/records.hrl").

-import_all(helpers).
-import_all(server).
-import_all(node3).

start() ->

	Server = spawn(fun() -> node3:server_setup() end),
	Intermidiate = spawn(fun() -> node3:client_setup(Server, Server) end),
	Client = spawn(fun() -> node3:client_setup(Server, Server) end),

	%Server ! { setup },
	%Intermidiate ! {setup, [Server, Client]}, 
	%Client ! {setup, [Server, Intermidiate]} ,

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
