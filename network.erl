-module(network).

-include_lib("headers/records.hrl").

-export([join/2]).

join(S, Pid) ->
	case length(S#state.children) =< S#state.capacity  of
		true -> 
			accept(S, Pid);
		false -> 
			refuse(S, Pid)
	end.

accept(S, Pid) -> 
	Pid ! { join_ok, S},
	S#state {
		children = S#state.children ++ [Pid]
	}.

refuse(S, Pid) ->
	Pid ! { join_refuse, lists:nth(rand:uniform(length(S#state.children)), S#state.children) },
	S.
