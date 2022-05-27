-module(buffer).

-include_lib("headers/records.hrl").

-export([run/1]).


run( L) ->
    receive 
		{ store, Data } -> 
            run([L | Data])
    end.