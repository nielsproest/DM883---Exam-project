-module(app).
-export([start/0]).

-import_all(group).

start() ->

	Streamer = spawn(fun() -> group:create(10) end ),

    connect_clients(Streamer, 200),

    spawn(fun() -> group:join(Streamer, fun(Data) -> 
            io:format("~p received ~c\n", [self(), Data]) 
        end )   
    end ),

    timer:sleep(1000),

    group:stream(Streamer, "Never gonna give you up..."),

	ok.


% Connect an abitrary number of clients to a given group streamer
connect_clients(_, 0) -> ok;
connect_clients(Streamer, N) ->
    spawn(fun() -> group:join(Streamer) end),
    connect_clients(Streamer, N - 1).

