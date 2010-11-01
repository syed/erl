-module(event_manager).
% Starting and stopping event handler 
-export([start/2,stop/1]).
% Initialize the event handler 
-export([init/1]).
% Exposed functions by event ahndler 
-export([add_handler/3,delete_handler/2,get_data/2,send_event/2]).



% Start event handler With handler list 
% Handler List will have [ { Handler , InitData} | .... ]

start(Name,HandlerList)->
    register(Name,spawn(event_manager,init,[HandlerList])).


%initialize event manager Call initalize for each handler and 
% Just do a loop with State Data as the Initalization list 

init(HandlerList)->
    loop(initialize(HandlerList)).

% initialize the handlers
% Call init of all the handlers 
initialize([])->
    [];
initialize([{Handler,InitData} | Tail ]) ->
    [{Handler,Handler:init(InitData)}| initialize(Tail)].


%stops the event manager 
stop(Name)->
    Name ! {stop,self()},
    receive 
	{reply,Reply} ->
	Reply 
    end.

%terminates the event handler 
% call the terminate function for each event handler 

terminate([])->
    [];
terminate([{Handler,Data}| Tail]) ->
    [{Handler,Handler:terminate(Data)}|terminate(Tail)].






% core API exposed by the event manager

% add a new handler to the event manager 
add_handler(Name,Handler,InitData)->
    call(Name,{add_handler,Handler,InitData}).

%delete an existing handler 
delete_handler(Name,Handler)->
    call(Name,{delete_handler,Handler}).

%get data from handler 
get_data(Name,Handler)->
    call(Name,{get_data,Handler}).

% send some event to the manager
send_event(Name,Event)->
    call(Name,{send_event,Event}).


% handlers different kinds of messages to the event manager 
handle_msg({add_handler,Handler,InitData},LoopData)->
    {ok , [{Handler,Handler:init(InitData)} | LoopData] };
handle_msg({delete_handler,Handler} , LoopData) ->
    case lists:keysearch(Handler,1,LoopData) of 
	false->
	    {{error , instance} , LoopData} ;
	{value , {Handler,Data}} ->
	    Reply = {data,Handler:terminate(Data)},
	    NewLoopData = lists:keydelete(Handler,1,LoopData),
	    {Reply,NewLoopData}
    end;
handle_msg({get_data,Handler} , LoopData) ->
    case lists:keysearch(Handler,1,LoopData) of 
	false->
	    {{error,instance},LoopData};
	{value , { Handler,Data}} ->
	    {{data,Data},LoopData}
    end;
handle_msg({send_event,Event} , LoopData) ->
    {ok , event(Event,LoopData)}.


% Calls handle_event for all the handlers 
event(_Event, [])->
    [];
event(Event,[{Handler,Data}| Rest] )->
[{Handler,Handler:handle_event(Event,Data)}| event(Event,Rest)].


% Wrapper around to send message to event manager 
call(Name,Mesg) ->
    Name ! { request , self(),Mesg},
    receive { reply,Reply}->
	    Reply 
    end.


% Wrapper to send reply 
reply(To,Mesg)->
    To!{reply,Mesg}.


% Main loop that the manager sits in 
loop(State)->
    receive 
	{ request , From , Mesg } ->
	    {Reply,NewState}= handle_msg(Mesg , State),
	    reply(From,Reply),
	    loop(NewState);
	{ stop , From } ->
	    reply(From , terminate(State))
    end.
