-module(test).
-export([test/0]).

-import_all(group).

% Single node test
test_to_one() ->
	io:format("test_to_one\n"),
	First = spawn(fun() -> ngroup:create(1) end),

    spawn(fun() -> ngroup:join([ { First, 1 }], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.

% Many nodes (outer layer) test
test_to_many() ->
	io:format("test_to_many\n"),
	First = spawn(fun() -> ngroup:create(1) end),

    connect_clients([ { First, 1 }], 10, []),

    spawn(fun() -> ngroup:join([ { First, 1 }], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.

% Test with many nodes
test_to_very_many() ->
	io:format("test_to_very_many\n"),
	First = spawn(fun() -> ngroup:create(1) end),

    connect_clients([ { First, 1 }], 1000, []),

    spawn(fun() -> ngroup:join([ { First, 1 }], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.

% Test with many nodes
test_from_many() ->
	io:format("test_from_many\n"),
	First = spawn(fun() -> ngroup:create(1) end),
	Second = spawn(fun() -> ngroup:create(2) end),

    connect_clients([ { First, 1 }, { Second, 2 } ], 1000, []),

    spawn(fun() -> ngroup:join([{ First, 1 },{Second, 2}], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),
    spawn(fun() ->  ngroup:stream(Second, 2, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.

% Test with mixed nodes
test_mixed() ->
	io:format("test_mixed\n"),
	First = spawn(fun() -> ngroup:create(1) end),
	Second = spawn(fun() -> ngroup:create(2) end),

    connect_clients([ { First, 1 }, { Second, 2 } ], 231, []),
    connect_clients([ { First, 1 } ], 238, []),
    connect_clients([ { Second, 2 } ], 312, []),

    spawn(fun() -> ngroup:join([{ First, 1 },{Second, 2}], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),
    spawn(fun() ->  ngroup:stream(Second, 2, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.

% Test with mixed nodes 2
test_mixed2() ->
	io:format("test_mixed2\n"),
	First = spawn(fun() -> ngroup:create(1) end),
	Second = spawn(fun() -> ngroup:create(2) end),

    connect_clients([ { First, 1 }, { Second, 2 } ], 231, []),
    connect_clients([ { First, 1 } ], 238, []),
    connect_clients([ { Second, 2 } ], 312, []),

    spawn(fun() -> ngroup:join([{ First, 1 }], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),
    spawn(fun() ->  ngroup:stream(Second, 2, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.

kill_random(_, 0, _) -> ok;
kill_random(Delay, N, Nodes) ->
	timer:sleep(Delay),
	RandomNode = lists:nth(rand:uniform(length(Nodes)),Nodes),
	RandomNode ! { die },
	%io:format("Kill ~p of ~p\n", [RandomNode, N]),
	kill_random(Delay, N-1, lists:filter(fun(X) -> X /= RandomNode end, Nodes)).

% Test to kill many nodes (bad case)
% Rarely fails
test_death() ->
	io:format("test_death\n"),
	First = spawn(fun() -> ngroup:create(1) end),

    Clients = connect_clients([ { First, 1 }], 1000, []),

    spawn(fun() -> ngroup:join([ { First, 1 }], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    spawn(fun() -> kill_random(10, 800, Clients) end),
	% will kill 300 before it runs
    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.


% Test to kill many nodes (worst case)
% Fails often
test_ultradeath() ->
	io:format("test_ultradeath\n"),
	First = spawn(fun() -> ngroup:create(1) end),

    Clients = connect_clients([ { First, 1 }], 1000, []),

    spawn(fun() -> ngroup:join([ { First, 1 }], fun(Data, StreamId) -> 
            io:format("~p received ~p\n", [StreamId, Data]) 
        end )   
    end ),

    spawn(fun() -> kill_random(5, 900, Clients) end),
	% will kill 600 before it runs
    timer:sleep(3000),

    spawn(fun() ->  ngroup:stream(First, 1, string:tokens("a b c d e f g h i j k l m n o p q r s t u v x y z æ ø å", [$\s, $\r, $\n])) end),

    timer:sleep(5000),
    ok.


test() ->
	test_to_one(),
	test_to_many(),
	test_to_very_many(),
	test_from_many(),
	test_mixed(),
	test_mixed2(),
	test_death(),
	test_death(),
	test_death(),
	test_ultradeath().

% Connect an abitrary number of clients to a given group streamer
connect_clients(_, 0, Pids) -> Pids;
connect_clients(Streamer, N, Pids) ->
    Pid = spawn(fun() -> ngroup:join(Streamer, fun(_, _) -> ok end) end),
    connect_clients(Streamer, N - 1, [ Pid | Pids]).
