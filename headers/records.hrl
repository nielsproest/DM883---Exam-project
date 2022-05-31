-type stream() :: integer().


% Message type that is sent between nodes
-record(message, {
	data :: any(),
    timestamp :: integer(),
    stream :: stream()
}).

% State kept within each node.
-record(state, {
	source :: #{stream() => pid()}, 
	timestamp :: #{stream() => integer()},
	version :: #{stream() => integer()},
	neighbours :: #{stream() => [pid()]},
	backflow :: #{stream() => [any()]},
	nodes :: #{stream() => [pid()]},
	capacity :: integer(),
	max_capacity :: integer()
}).
