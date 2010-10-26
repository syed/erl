-module(fsm).
-export([start/0,stop/0]).
-export([wait/0,signal/0]).
-export([init/0]).


%simple mutex implementation as a fsm 

% wait will acquire the lock 
% signal will release it 

start()->
    % ?MODULE is a predefined value like a #define thing
    register(mutex,spawn(?MODULE,init,[])).

stop()->
    mutex ! stop.

wait()->
    mutex ! { wait , self()},
    receive 
	ok ->
	    ok 
    end.

signal()->
    mutex ! { signal , self()},
    receive 
	ok ->
	    ok 
    end.

init()->
    free().

free()->
    receive ->
	    {wait , Pid } ->
		    Pid!ok,
		    busy(Pid);
	    stop -> terminate()
    end.

busy(Pid) ->
    receive
	{signal,Pid} ->
	    free()
    end.

terminate() ->
    receive 
	{ wait , Pid } ->
	    exit(Pid,kill),
	    terminate()
    after
	0 ->
	    ok
    end.


