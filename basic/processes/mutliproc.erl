-module(multiproc).

-export([sleep/1,flush/0]).


sleep(TimeMs) ->
    receive 
    after TimeMs ->
           ok
    end.

flush() ->
    receive
        _ -> flush() 
    after 0 ->
        ok 
    end.


important() -> 
    receive 
        { _From, Priority, Msg } when Priority > 10 ->
            [ Msg | important() ] 
    after 0 ->
            normal()
    end.

normal() ->
    receive 
        { _From,Prority, Msg } -> 
            [ Msg | normal() ]
    after 0 -> 
            []
    end.
