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



