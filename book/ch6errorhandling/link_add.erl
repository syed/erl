-module(link_add).
-export([start/0,request/1,loop/0]).


start()->
    register(link_add_proc,spawn_link(link_add,loop,[])).
request(Int) ->
    link_add_proc ! { request , self() , Int } ,
    receive 
	{ result , Result } ->
	    Result
    after 1000 ->
	    timeout 
    end.

loop() ->
    receive 
	    {request,Pid,Msg} ->
		    Pid ! { result , Msg+1}
	    end,
    loop().

	    
