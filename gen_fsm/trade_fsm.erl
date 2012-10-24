%% example of a simple trading system from LYSE
%% http://learnyousomeerlang.com/finite-state-machines

-module(trade_fsm).
-behaviour(gen_fsm).

%% API 

-export([start/1,start_link/1,trade/2,accept_trade/1,
            make_offer/2,retract_offer/2,ready/1,cancel/1]).

%% gen_fsm callbacks + our state callbacks
-export( [init/1,handle_event/3,handle_sync_event/4,handle_info/3,
            terminate/3,code_change/4,
         % handler callbacks 
            idle/2,idle/3,idle_wait/2,idle_wait/3,negotiate/2,
            negotiate/3,wait/2, ready/2, ready/3] ). 
        % here the fun/3 are for async calls and fun/2 are for sync calls


-record(state, { name="",
                other,
                ownitems=[],
                otheritems=[],
                monitor,
                from}).


%% Implementing the API 

% These functions are called by the client
% to the FSM .All of them are sync functions
% so that the client waits if we are in some
% wait state. They will unblock when someone
% in the functions sends gen_fsm:reply(Pid,Data)

start(Name)-> gen_fsm:start(?MODULE,[Name],[]).
start_link(Name)-> gen_fsm:start_link(?MODULE,[Name],[]).


% ask someone to trade  
trade(OwnPid,OtherPid)->
    gen_fsm:sync_send_event(OwnPid, { negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid,accept_negotiate).

% send an item to be traded
make_offer(OwnPid,Item) ->
    gen_fsm:send_event(OwnPid,{make_offer,Item}).

% cancel trade offer 
retract_offer(OwnPid,Item) ->
    gen_fsm:send_event(OwnPid,{ retract_offer, Item}).

% tell that we are ready to other peer 
ready(OwnPid)->
    gen_fsm:send_sync_event(OwnPid,ready,infinity).

cancel(OwnPid)->
% cancel transaction
    gen_fsm:sync_send_all_state_event(OwnPid,cancel).
    
%% functions for fsm to fsm communication 
%% these functions are async 

% ask for other fsm if it is ready to trade
ask_negotiate(OtherPid,OwnPid)->
    gen_fsm:send_event(OtherPid,{ask_negotiate,OwnPid}).

% send to other fsm that we are ready to trade
accept_negotiate(OtherPid,OwnPid) ->
    gen_fsm:send_event(OtherPid,{accept_negotiate,OwnPid}).

% forward client's offer
do_offer(OtherPid,Item) ->
    gen_fsm:send_event(OtherPid,{do_offer,Item}).

% forward clients offer cancellation
undo_offer(OtherPid,Item) ->
    gen_fsm:send_event(OtherPid,{undo_offer,Item}).

% ask other fsm if he is in ready state
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid,are_you_ready).

% reply that we are not yet ready 
not_yet(OtherPid) -> 
    gen_fsm:send_event(OtherPid,not_yet).

% reply that we are waiting for the ready from 
% other side 
am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid,'ready!').

% Ack that fsm is in ready state
ack_trans(OtherPid)->
    gen_fsm:send_event(OtherPid,ack).

% ask if ready to commit 
ask_commit(OtherPid)->
    gen_fsm:sync_send_event(OtherPid,ask_commit).

% start commit 
do_commit(OtherPid)->
    gen_fsm:sync_send_event(OtherPid,do_commit).

% other guy cancelled, notify our guy
notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid,cancel).


%% Implementing the gen_fsm functions

init(Name) ->
    {ok,idle,#state{name=Name}}.  % goes to idle state

% we come here when someone asks us to trade,
% he himself will move to idle_wait,we too move to idle_wait
% this is async 
idle({ask_negotiate, OtherPid} , S=#state{}) ->
    Ref=monitor(process,OtherPid),
    notice(S,"~p asked for trade negotioation", [OtherPid]),
    { next_state, idle_wait, 
        S#state{other=OtherPid, monitor=Ref}};

% handling unexpected event
idle(Event,State) ->
    unexpected(Event,idle),
    {next_state,idle,State}.

% we come here when our user asks us to trade with some other
% guy we will move to idle_wait and call ask_negotiate
% which will send an event to OtherPid.
% from is used to store our pid if we want to send messages
% later

idle({negotiate,OtherPid} ,From, S=#state{}) ->
    ask_negotiate(OtherPid,self()),
    notice(S,"asking user ~p for trade",[OtherPid]),
    Ref=monitor(process,OtherPid),
    {next_state, idle_wait, S#state{other=OtherPid,monitor=Ref,
            from=From}};

% handling unexpected event
idle(Event,_From, Data) ->
    unexpected(Event,idle),
    {next_state,idle,Data}.


% we come in idle_wait when 
%  1. we sent a request to negotiate 
%  2. we recieved a request to negotiate and
%     send the response
%  it is also possible that we sent a request to
%  trade, move to idle_wait state and at the same
%  time the other peer also did the same thing. In 
%  this case we can get a request to negotiate
%  in idle_wait state. 

% this is a race condition. if we ask some one to trade
% and he asks us to trade at the same time, both will
% be in idle_wait state. and both will get ask_negotiate, 
% in this case we unblock our client and move to 
% negotiate 
idle_wait({ask_negotiate,OtherPid} , S=#state{other=OtherPid}) ->
    gen_fsm:reply(S#state.from,ok), % this unblocks the client
                                  % which is waiting in sync call
    notice(S,"starting negotiation",[]),
    {next_state,negotiate,S};

% if someone accepts our negotiation, we reply to the
% client this will unlock the client which is waiting 
% in a sync call
idle_wait({accept_negotiate,OtherPid}, S=#state{other=OtherPid}) ->
    gen_fsm:reply(S#state.from,ok),
    notice(S,"starting negotiatin",[]),
    {next_state,negotiate,S};


idle_wait(Event,Data) -> 
    unexpected(Event,idle_wait),
        { next_state,idle_wait,Data }.


% sync functions 
% this gets called when the client does accept_trade
% we will move to negotiate state and reply client with
% ok 
idle_wait(accept_negotiate , _From, S=#state{other=OtherPid})->
    accept_negotiate(OtherPid,self()),
    notice(S,"acccepting negotiation",[]),
    { reply , ok, negotiate,S};
idle_wait(Event,_From,Data) ->
    unexpected(Event,idle_wait),
    {next_state,idle_wait,Data}.


% negotiate state is where both sides have accepted the trade
% and are ready to trade items

negotiate({make_offer,Item} , S=#state{ownitems=OwnItems}) ->
    do_offer(S#state.other,Item),
    notice(S,"offering ~p",[Item]),
    {next_state,negotiate,S#state{ownitems=add(Item,OwnItems)}};

negotiate({retract_offer,Item},S=#state{ownitems=OwnItems})->
    undo_offer(S#state.other,Item),
    notice(S,"Cancelling offer ~p",[Item]),
    {next_state,negotiate,S#state{ownitems=remove(Item,OwnItems)}};
negotiate({do_offer,Item},S=#state{otheritems=OtherItems})->
    notice(S,"other player offering ~p",[Item]),
    {next_state,negotiate,S#state{otheritems=add(Item,OtherItems)}};
negotiate({undo_offer,Item},S=#state{otheritems=OtherItems}) ->
    notice(S,"Other palyer cancelling offer ~p",[Item]),
    {next_state,negotiate,S#state{otheritems=remove(Item,OtherItems)}};

% we can get are_you_ready event in negotiate saying that
% the other guy is ready to finish the trade and are you ready?
% we will just reply that we are not ready. 
% We will act on ready event when we are
% done with negotiation and we move to wait state.
%

negotiate(are_you_ready, S=#state{other=OtherPid})->
    io:format("Other user ready to transfer~n"),
    notice(S,
        "Other user ready to transfer goods:~n"
        "You get ~p, The other side gets ~p",
        [S#state.otheritems,S#state.ownitems]),
    not_yet(OtherPid),
    {next_state,negotiate,S};

negotiate(Event,Data) ->
    unexpected(Event,negotiate),
    {next_state,negotiate,Data}.

% client telling that he is ready
negotiate(ready,From,S=#state{other=OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S,"asking if ready, waiting",[]),
    {next_state,wait,S#state{from=From}};
negotiate(Event,_From,S)->
    unexpected(Event,negotiate),
    {next_state,negotiate,S}.

% wait state is the one where we are done with 
% adding items and we are ready to transfer the
% items and waiting on other side to be ready.
% here if other side is still offering items
% we should move back to negotiate

wait({do_offer,Item},S=#state{otheritems=OtherItems})->
    gen_fsm:reply(S#state.from,offer_changed),
    notice(S,"other side offering ~p",[Item]),
    {next_state,negotiate,S#state{otheritems=add(Item,OtherItems)}};
wait({undo_offer,Item},S=#state{otheritems=OtherItems})->
    gen_fsm:reply(S#state.from,offer_changed),
    {next_state,negotiate,S#state{otheritems=remove(Item,OtherItems)}};

% if we are in wait state and other guy is
% asking if we are ready we can reply saiying
% i am ready but we don't change state here 

wait(are_you_ready,S=#state{})->
    am_ready(S#state.other),
    notice(S,"asked if ready, I am. Waiting for same reply",[]),
    {next_state,wait,S};

% in this case client moved to ready but other side
% replied with not yet. We will be in wait  state 

wait(not_yet,S=#state{}) ->
    notice(S,"Other not ready yet",[]),
    {next_state,wait,S};

% here we are in wait and other guy replied with I am ready
% we can start the transfer of goods 

wait('ready!' , S=#state{})->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from,ok),
    notice(S,"other side is ready , moving to ready",[]),
    {next_state,ready,S}.


% moving to ready state ,we initate the two phase commit

ready( ack , S=#state{}) ->
    case priority(self(),S#state.other) of 
        true  -> 
            try
                notice(S,"asking for commit",[]),
                read_commit = ask_commit(S#state.other),
                notice(S,"ordering commit",[]),
                ok = do_commit( S#state.other),
                notice(S,"committing ...",[]),
                commit(S),
                {stop,normal,S}
            catch Class:Reason ->
                notice(S,"commit failed",[]),
                {stop,{Class,Reason},S}
            end;
        false->
            {next_state,ready,S}
    end;


ready(Event,Data)->
    unexpected(Event,ready),
    {next_state,ready,Data}.

ready(ask_commit, _From, S) ->
    notice(S,"replying to ask_commit",[]),
    {reply,ready_commit,ready,S};
ready(do_commit,_From,S)->
    notice(S,"committing...",[]),
    {stop,normal,ok,S};
ready(Event,_From,Data)->
    unexpected(Event,ready),
    {next_state,ready,Data}.

commit(S = #state{}) ->
    io:format("Transaction completed for ~s. "
        "Items sent are:~n~p,~n received are:~n~p.~n"
        "This operation should have some atomic save "
        "in a database.~n",
        [S#state.name, S#state.ownitems, S#state.otheritems]).

% other player cancelled his tranaction 
% we should do the same

handle_event(cancel, _StateName, S=#state{}) ->
    notice(S, "received cancel event", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state,StateName,Data}.


% this gets called when our client cancels the trade
% we have to send an notification to the other FSM 
% to cancel that side too

handle_sync_event(cancel,_From,_StateName, S=#state{})->
    notify_cancel(S#state.other),
    notice(S,"cancelling trade,sending cancel event",[]),
    {stop,cancelled,ok,S};

handle_sync_event(Event,_from,StateName,Data)->
    unexpected(Event,StateName),
    { next_state,StateName,Data}.

% function for handling monitor messages 

handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) ->
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.


code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

% called when transaction is completed

terminate(normal, ready, S=#state{}) ->
    notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.

% function which decides which FSM will start the first 
% transaction for a two-phase commit

priority(OwnPid,OtherPid)  ->
    if 
        OwnPid > OtherPid -> true;
        OwnPid < OtherPid -> false
    end.

% adding and removing items

add(Item,Items) ->
    [Item| Items].
remove(Item,Items)->
    Items -- [Item].

%% Logging functions

% prints a notice
notice(_S=#state{name=N}, Str, Args)->
    io:format("~s: "++Str++"~n",[N|Args]).

% for unexpected messages
unexpected(Msg,State) ->
    io:format("~p received unknown event ~p when in state ~p~n",
        [self(),Msg,State]).


