## Welcome to the exam project for DM883: Distributed Systems

## To building the project
To compile all files in root do.
````
erl -make
````

## Quick start
This is the fastest way to get the app up and running.
````
erl -make
erl -pa .\bin\
app:start().
````

## To test the program, run the following functions:
The output will be printed out in the terminal. 
````
erl -make
erl -pa .\bin\
test:test().
````
This runs multiple tests, including kill tests, where a majority of nodes are killed (this sometimes fails, as it is the worst case).
This include one to many, many to many, kill test etc.

## API
To spawn a new broadcaster, you run:
````
ngroup:create(1)
````
This creates a broadcaster of id 1.
This action is however blocking, so you may wish to spawn a seperate process with it
````
Broadcast = spawn(fun() -> ngroup:create(1) end)
````
When this broadcaster is created, you may now create nodes who want the data.
````
ngroup:join([{Broadcast, 1}], fun(Data, StreamId) -> ok end )
````
This then adds a new node to the network of "Broadcast", and it also requires you to specify the broadcast id, which was 1.
This action is also blocking, so putting it in a spawn makes the most sense.
The node can specify multiple streams to join, with tuples. 
It then receives data in an anonymous function with the corresponding SteamId.

To then finally stream some data to recipients, you run:
````
ngroup:stream(Broadcast, 1, [1,2,3,4])
````
Which will send the data "1,2,3, and 4" from the broadcaster as StreamId 1.
