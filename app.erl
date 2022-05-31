-module(app).
-export([start/0]).

-import_all(group).

start() ->
	Streamer = spawn(fun() -> ngroup:create(1) end),

    %Clients = connect_clients(Streamer, 1, []),

    %io:format("Clients: ~p \n", [Clients]),

   % spawn(fun() -> group:join(Streamer, fun(Data) -> 
   %         io:format("~p received ~p\n", [self(), Data]) 
   %     end )   
   % end ),

    timer:sleep(3000),

    {ok, File} = file:read_file("data.txt"),

    Content = unicode:characters_to_list(File),

    % 
    ngroup:stream(Streamer, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])),

    ok.

% Connect an abitrary number of clients to a given group streamer
connect_clients(_, 0, Pids) -> Pids;
connect_clients(Streamer, N, Pids) ->
    Pid = spawn(fun() -> ngroup:join(Streamer, fun(_) -> ok end) end),
    connect_clients(Streamer, N - 1, [ Pid | Pids]).

