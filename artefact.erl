
-record(state, {
	neighbors :: [],
}).

send_msg([], _) -> ok;
send_msg([{Pid} | Rest], Msg) ->
	Pid ! Msg,
	send_msg(Rest, Msg).

send_data(S, Data) ->
	send_msg(S#neighbors, {recv, Data})

-spec(loop(#state{}) -> no_return()).
loop(S) -> 
	receive
		% Start (dummy)
		start -> loop(S);
		% Set state
		{ state, _S } -> loop(_S);

		{ recv, Data } -> 
			io:format("~p was received!\n", [Data]);

		_ -> error % Wrong command
	after 2000 -> error % Inactivity
	end.

gen_state(Neighbor) -> 
	#state {
		neighbors = Neighbor
	}.

server_proc(nodes) -> 
    {ok,[N]} = io:fread("","~d"),
	send_msg(nodes, {recv, N}),
	server_proc(nodes).

start() ->
	Pid1 = spawn(fun() -> loop(#state{}) end),
	Pid2 = spawn(fun() -> loop(#state{}) end),
	Pid3 = spawn(fun() -> loop(#state{}) end),
	Pid1 ! {state, gen_state([Pid2,Pid5])},
	Pid2 ! {state, gen_state([Pid1,Pid3])},
	Pid3 ! {state, gen_state([Pid2,Pid4])},
	Pid1 ! start,
	Pid2 ! start,
	Pid3 ! start,
	server_proc([Pid1,Pid2,Pid3]).
