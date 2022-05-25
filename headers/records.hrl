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
    data :: [], %Buffer
	timestamp :: 0,
	parent :: pid(),
	children :: [pid()],
	capacity :: 5
}).
