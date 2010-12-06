-module(db_revisited).
-export([start/0,stop/0,init/0,write/2,read/1,delete/1]).


start()->
    register(db_server,spawn(db_revisited,init,[])).

stop()->
    call(stop).

init()->
    DBData = db:new(),
    loop(DBData).


loop(DBData) ->
    receive 
	{ request , From , {read,Key} }->
	    Res=db:read(Key,DBData),
	    reply(From,Res),
	    loop(DBData);
	{request , From , { write , Key , Value } } ->
	    NewDb=db:write(Key,Value,DBData),
	    Reply = { ok , Key },
	    reply(From,Reply),
	    loop(NewDb);
	{request , From , { delete , Key } } ->
	    NewDb=db:delete(Key,DBData),
	    Reply = { ok , Key  },
	    reply(From , Reply ),
	    loop(NewDb);
	stop ->
	    db:destroy(DBData),
	    ok;
        _Other -> 
	    io:format("Got ~p~n",[_Other])
       
    end.

reply(From,Mesg)->
    io:format("sendding reply to ~p  : ~p ",[From,Mesg]),
    From ! { reply , Mesg }.
	    

call(Message)->
    io:format("sending ~p ~n",[Message]),
    db_server ! { request , self() , Message },
    receive 
	{reply , Reply } ->
	    Reply
    end.


% Client functions Read , Write , Delete 

read(Key) ->
    call({read,Key}).

write(Key,Value)->
    call({write,Key,Value}).

delete(Key)->
    call({delete,Key}).

    
