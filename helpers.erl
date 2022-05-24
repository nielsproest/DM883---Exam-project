-module(helpers).
-export([generate_data/0]).



generate_data() ->
    rand:uniform(10).