% Message type that is sent between nodes
-record(message, {
	data :: byte(),
    sender :: pid(),
    timestamp :: integer(),
    stream :: integer()
}).

% State kept within each node.
-record(state, {
	source :: pid(), %Streamer
    data :: [], %Buffer
	parent :: pid(),
	children :: [pid()],
	capacity :: 5
}).
