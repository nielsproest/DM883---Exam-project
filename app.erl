-module(app).
-export([start/0]).

-include_lib("headers/records.hrl").

-import_all(helpers).
-import_all(server).
-import_all(node).
-import_all(group).

start() ->

	Streamer = spawn(fun() -> group:create() end ),

    connect_clients(Streamer, 8),

    timer:sleep(2000),

    stream(Streamer, "He"),

	ok.

stream(_, []) -> ok;
stream(Node, [ H | T]) -> 
    Node ! { packet, #message {
        data = H,
        sender = self(),
        timestamp = 0,
        stream = 0
    }},
    stream(Node, T).



connect_clients(_, 0) -> ok;
connect_clients(Streamer, N) ->
    spawn(fun() -> group:join(Streamer) end),
    connect_clients(Streamer, N - 1).

