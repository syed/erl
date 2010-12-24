-module(monitor_test).

-export([start/3,dummy/0]).

% This is an example to use monitor to fix the race condition 
% when creating a process and linking it later 
% given in the book pg 144


start(Module,Function,Args)->
    Pid = spawn(Module,Function,Args),
    Ref = erlang:monitor(process,Pid),
    receive 
	{ 'DOWN' , _Ref , process , _Pid , Reason } ->
	     { not_working , Reason }
    after
	100 ->
	    register(monitor_test,Pid),
	    io:format("linking pid ~p" , [Pid]),
	    link(Pid)
    end.


% dummy function sends what it recievs 

dummy()->
    receive
	{From , Data } ->
	    From ! Data ,
	    dummy();
	stop -> ok 
    end.

	    
