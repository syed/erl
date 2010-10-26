-module(echo).
-export([start/0,stop/0,print/1,server/0]).

start() ->
    Pid = spawn(echo , server , [] ),
    register(server_proc , Pid) ,
  %  {start} ! Pid ,
    ok .
stop()->
    server_proc ! stop  ,
    ok.

print(Mesg)->
     server_proc ! Mesg ,
    %io:format(Mesg),
    ok.


server()->
    receive 
	stop ->
	    ok ;
	Mesg ->
	    io:format(Mesg),
	    server()
    end.
    
