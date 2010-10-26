-module(processes).
-export([create_process/0,do_something/1]).


% Create a process which does something 

create_process() ->
    Pid  = spawn(processes,do_something,[self()]),
    io:format("Child created ~p~n" , [Pid]),
    Pid ! { self() , foo } ,
%   Child ! hello , % sending resturns the message sent useful in sending to many processes Pid1 ! ( Pid2 ! ( Pid3 ! Mesg ) ) 
    receive 
	{Pid2 , done } ->
	    io:format("Child done ~n"),
	    Pid ! { self() , done } ;
	_Other  -> 
	    io:format("Other got ~p~n" , [_Other]),
	    {error , no_match } 
    end. 

do_something(Parent) ->
    receive 
	{ Pid , foo } ->
	    Pid ! { self() , done };
	_Other -> 
	    io:format("Got other than foo ~p ~n" , [_Other])
    end.


	 




    


