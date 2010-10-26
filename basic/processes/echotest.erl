-module(echotest).
-export([go/0,loop/0]).

go() ->
    Pid = spawn(echotest , loop , [] ),
    Pid ! { self() , hello },
    receive
	{Pid , Msg} ->
	    io:format("~w~n",[Msg])
    end,
    Pid ! stop .


loop() ->
    receive 
	{ From , Msg} ->
	    From ! { self() , Msg },
	    loop();
	stop ->
	    true
    end .

	    
