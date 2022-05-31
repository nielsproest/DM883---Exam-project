-module(app).
-export([start/0]).

-import_all(group).

start() ->
	First = spawn(fun() -> ngroup:create(1) end),
    Second = spawn(fun() -> ngroup:create(2) end),

    connect_clients([ { First, 1 }, { Second, 2 }], 10, []),
    connect_clients([ { Second, 2 }], 5, []),
    connect_clients([ { First, 1 }], 5, []),

    %io:format("Clients: ~p \n", [Clients]),

    spawn(fun() -> ngroup:join([ { First, 1 }, { Second, 2}], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    timer:sleep(3000),

    {ok, File} = file:read_file("data.txt"),

    Content = unicode:characters_to_list(File),

    % 
    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d ", [$\s, $\r, $\n])) end),
    spawn(fun() ->  ngroup:stream(Second, 2, string:tokens("e f g h ", [$\s, $\r, $\n])) end),


    ok.

% Connect an abitrary number of clients to a given group streamer
connect_clients(_, 0, Pids) -> Pids;
connect_clients(Streamer, N, Pids) ->
    Pid = spawn(fun() -> ngroup:join(Streamer, fun(_, _) -> ok end) end),
    connect_clients(Streamer, N - 1, [ Pid | Pids]).

