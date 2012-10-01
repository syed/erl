-module(event).
-record(state, { server,
                 name="",
                 delay_list=0 } ).

-export([loop/1,start/2,start_link/2,init/3,cancel/1]).


% start and start_link are standard functions that are 
% called a part of app initilization , 

start(EventName,Delay) ->
    spawn(?MODULE,init,[self(),EventName,normalize(Delay)]).

start_link(EventName,Delay) ->
    spawn_link(?MODULE,init,[self(),EventName,normalize(Delay)]).

init(Server,Name,DateTime) ->
    loop(#state{server=Server,name=Name,delay_list=convert_datetime_to_seconds(DateTime)}).

% main loop for the event. we sleep till the delay time
% given during init and wakeup and send 
% a done message back to the event server
% we can also get a cancel request from server 
% for which we simply return

loop(S= #state{server=Server, delay_list=[T|Next]}) ->
    receive 
        {Server,Ref,cancel} -> 
            Server ! { ok, Ref }
    after T*1000 -> 
           if   Next =:= [] -> 
                    Server !  { done , S#state.name };
                Next =/= [] ->
                    loop( S#state{delay_list=Next} )
            end
    end.

% normalize divides the time into slices of 49 days. 
% We are doing this because erlang has a limit of  ~50 
% days on the 'after' clause. We return a list of 
% times which add up to the final time each with 
% max limit of 49 days .

normalize(Time) -> 
    Limit = 49*24*60*60,
    Num = Time div Limit,
    Rem = Time rem Limit,
    [Rem| lists:duplicate(Num,Limit)].

% cancel an event, wrapper around sending a message
% to the event process

cancel(Pid) -> 
    Ref = erlang:monitor(process,Pid),
    Pid ! { self(),Ref,cancel },
    erlang:demonitor(Ref,[flush]),
    receive 
        { ok, Ref } -> ok;
        {'DOWN', Ref, process, _Object, _Reason} -> ok
    end.


% converts time from { {Day,Mon,Year} { Hr,Min,Day} } to seconds + normalize it 

convert_datetime_to_seconds(Time={{_,_,_},{_,_,_}} ) ->
    Now = calendar:local_time(),
    RemTime =calendar:datetime_to_gregorian_seconds(Time) - calendar:datetime_to_gregorian_seconds(Now),
    Sec = if RemTime > 0 -> RemTime;
             RemTime =< 0 -> 0
        end,
    normalize(Sec).
