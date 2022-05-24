-module(server).
-export([handle/1]).


handle(Clients) -> io:format("~p \n", [Clients]).