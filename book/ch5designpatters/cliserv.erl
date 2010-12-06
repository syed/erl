-module(cliserv).
-export([start/0,stop/0,allocate/0,deallocate/1,init/0]).


%Create and initialize the frequency server

start()->
    register(freqserv , spawn(cliserv,init,[])).

init()->
    Frequencies = { get_frequencies() , [] },
    loop(Frequencies).

get_frequencies()->
    [10,11,12,13,14,15].


% MAIN Server Loop

loop(Frequencies) ->
    receive 
	{ request , Pid , allocate } ->
	    { NewFrequencies , Reply } = allocate(Frequencies , Pid ),
	    reply(Pid,Reply),
	    loop(NewFrequencies);
	{ request , Pid , { deallocate , Freq }} ->
	    NewFrequencies = deallocate(Frequencies , Freq ),
	    reply(Pid,ok),
	    loop(NewFrequencies);
	{request,Pid,stop} ->
	    reply(Pid,ok)
    end.
	      
reply(Pid,Reply)->
    Pid ! {reply,Reply}.


allocate({[],Allocated},_Pid)->
    {{[],Allocated},{error,no_frequency}};
 allocate({[Freq|Free] , Allocated} , Pid ) ->
    {{Free,[{Freq,Pid} | Allocated]},{ok,Freq}}.

deallocate({Free,Allocated} , Freq)->
    NewAllocated = lists:keydelete(Freq,1,Allocated),
    {[Freq|Free],NewAllocated}.


%client functions to the server
%all functions hidden from the user
stop() ->
    call(stop).

allocate() ->
    call(allocate).

deallocate(Freq) ->
    call({deallocate,Freq}).

call(Message) ->
    freqserv ! { request , self() , Message },
    receive 
	{ reply , Reply } ->
	    Reply 
    end.
