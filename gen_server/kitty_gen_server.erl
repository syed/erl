% Examples from 
% http://learnyousomeerlang.com/clients-and-servers

-module(kitty_gen_server).
-behaviour(gen_server).

-record(cat, { name, color,description}).

% exports for cat API
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

% exports  for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                 terminate/2, code_change/3]).


start_link() -> gen_server:start_link(?MODULE,[],[]).


% Sync API, Here some client process orders a call
% we will check our DB and return the first cat 
% we find. This will be sync call. The client will be 
% blocked until a reply is received or timeout happens

order_cat(Pid,Name,Color,Description) ->
    gen_server:call(Pid,{order,Name,Color,Description}).

% Async API. This call is called by the client if
% it wants to return a cat back. We will put the 
% cat in DB and return OK

return_cat(Pid,Cat=#cat{})->
    gen_server:cast(Pid,{return,Cat}).

% this terminates the cat server

close_shop(Pid) ->
    gen_server:call(Pid,terminate).


% gen_server behaviour functions 

init([])-> { ok , [] }.

% handle_call for sync calls
handle_call({ order, Name, Color, Description} , _From, Cats) ->
    if Cats =:= [] ->
            { reply , make_cat(Name,Color,Description), Cats };
        Cats =/= [] ->
            { reply, hd(Cats) , tl(Cats) }
    end;

handle_call(terminate,_From,Cats)->
    {stop,normal,ok,Cats}.


% handle_cast for async calls
handle_cast({return, Cat= #cat{}}, Cats) ->
    { noreply,[Cat|Cats]}.

% handle_info for unknwon calls
handle_info(Msg,_Cats)->
    io:format("Unexpected message ~p~n",[Msg]).

terminate(normal,Cats) ->
    [io:format("~p was set free.~n",[C#cat.name])|| C<-Cats],
    ok.

code_change(_OldVsn, State, _Extra)->
    {ok,State}.

% END gen_server behaviour

%% Private functions here

make_cat(Name,Color,Description)->
    #cat{name=Name,color=Color,description=Description}.

