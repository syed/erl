-module(link_add).
-export([start/0,request/1,loop/0]).


start()->
    process_flag(trap_exit,true),
    Pid=spawn_link(link_add,loop,[]),
    register(link_add_proc,Pid),
    {ok,Pid}.


request(Int) ->
    link_add_proc ! { request , self() , Int } ,
    receive 
	{ result , Result } ->
	    Result;
	{ 'EXIT' , _Pid , Reason } -> { error , Reason }
       
    after 1000 ->
	    timeout 
    end.

loop() ->
    receive 
	    {request,Pid,Msg} ->
		    Pid ! { result , Msg+1}
	    end,
    loop().

	    
