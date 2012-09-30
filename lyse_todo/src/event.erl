-module(event).
-record(state, { server,
                 name="",
                 delay=0 } ).

-export([loop/1]).


loop(S= #state{server=Server }) ->
    receive 
        {Server,Ref,cancel} -> 
            Server ! { ok, Ref }
    after (S#state.delay)*1000 ->
            Server !  { done , S#state.name }
    end.



