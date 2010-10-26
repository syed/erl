-module(event_manager).
-export([start/2,stop/1,add_handler/3,delete_handler/2,get_data/2,send_event/2]).
-export([init/1]).

start(Name,HandlerList)->
    register(Name,spawn(event_manager,init,[HandlerList]).


%initialize event manager
init(HandlerList)->
    loop(initialize(HandlerList)).

% initialize the handlers
initialize([])->
    [];
initialize([{Handler,InitData} | Tail ]) ->
    [{Handler,Handler:init(InitData)}| initialize(Tail)].

%stops the event manager 
stop(Name)->
    Name ! {stop,self()},
    receive 
	{reply,Reply}
	Reply 
    end.

%terminates the event handler 
terminate([])->
    [];
terminate([{Handler,Data}| Tail]) ->
    [{Handler,Handler:terminate(Data)}|terminate(Tail)].






% core API exposed by the event manager

add_handler(Name,Handler,InitData)->
    call(Name,{add_handler,Handler,InitData}).

delete_handler(Name,Handler)->
    call(Name,{delete_handler,Handler}).

get_data(Name,Handler)->
    call(Name,{get_data,Handler}.

send_event(Name,Event)->
    call(Name,{send_event,Event}).

handler_msg({add
