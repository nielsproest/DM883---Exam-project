-module(pinger).
-export([run/1]).

% Running nodes continously wait for new packets to handle.
run(Server) ->

	loop(Server).

loop(Parent) -> Parent ! { update_neighbours }, timer:sleep(1000), loop(Parent).