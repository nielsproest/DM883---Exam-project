-module(app).
-export([start/0]).

-import_all(group).

start() ->

	Streamer = spawn(fun() -> group:create(1) end ),

    Clients = connect_clients(Streamer, 1, []),

    io:format("Clients: ~p \n", [Clients]),

    spawn(fun() -> group:join(Streamer, fun(Data) -> 
            io:format("~p received ~p\n", [self(), Data]) 
        end )   
    end ),

    P = lists:nth(rand:uniform(length(Clients)), Clients),

    {ok, File} = file:read_file("data.txt"),

    Content = unicode:characters_to_list(File),

    group:stream(Streamer, string:tokens("Hej med dig", [$\s, $\r, $\n])),

    io:format("kill: ~p\n", [P]),
    exit(P, kill),

    %group:stream(Streamer, string:tokens(Content, [$\s, $\r, $\n], Clients)),
    % 
    group:stream(Streamer, string:tokens("jeg hedder kaj dette er en lang besked wtf hahe hs hs hdh sdd", [$\s, $\r, $\n])),

    ok.

% Connect an abitrary number of clients to a given group streamer
connect_clients(_, 0, Pids) -> Pids;
connect_clients(Streamer, N, Pids) ->
    Pid = spawn(fun() -> group:join(Streamer) end),
    connect_clients(Streamer, N - 1, [ Pid | Pids]).

