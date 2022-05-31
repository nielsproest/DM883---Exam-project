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
This runs multiple tests, some of which are expected to fail.
This include one to many, many to many, kill test etc.
