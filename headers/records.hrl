% Message type that is sent between nodes
-record(message, {
	data :: any(),
    sender :: pid(),
    timestamp :: integer(),
    stream :: integer()
}).

% State kept within each node.
-record(state, {
	source :: pid(), %Streamer
	timestamp :: integer(),
	version :: integer(),
	neighbours :: [pid()],
	backflow :: [],
	nodes :: [pid()],
	capacity :: integer(),
	max_capacity :: integer()
}).
