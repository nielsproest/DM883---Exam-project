-module(utility).

-export([send_msg/2, n_random/2, lists_first/1, lists_last/1, n_exclude/2]).


lists_first([]) -> ok;
lists_first([H | _]) ->
	H.

lists_last(L) -> lists_last(L, ok).
lists_last([], T) -> T;
lists_last([H | T], _) ->
	lists_last(T, H).


send_msg([], _) -> ok;
send_msg([H | T], Msg) ->
    H ! Msg,
    send_msg(T, Msg).

n_exclude(HT,X) -> n_exclude(HT,[],X).
n_exclude([],L,_) ->
	L;
n_exclude([H | T], L, X) when H == X ->
	n_exclude(T, L, X);
n_exclude([H | T], L, X) ->
	n_exclude(T, L ++ [H], X).

n_random(N, List) -> n_random(N, [], List).
n_random(0, Out, _) ->
	lists:usort(Out);
n_random(N, Out, List) ->
	Random_client = lists:nth(rand:uniform(length(List)), List),
	n_random(N-1, Out ++ [Random_client], List).
