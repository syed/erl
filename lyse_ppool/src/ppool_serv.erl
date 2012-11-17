-module(ppool_serv).
-behaviour(gen_server).

-export( [start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).

-export( [init/1, handle_call/3, handle_cast/2, handle_info/2,
            code_change/3, terminate/2]).

-define(SPEC(MFA),
    {worker_sup,
        { ppool_worker_sup, start_link, [MFA]},
            permanent,
            10000,
            supervisor,
            [ppool_worker_sup]}).

-record(state, { limit=0,
                 sup,
                 refs,
                 queue=queue:new()}).


% client api     

start(Name, Limit, Sup, MFA) when is_atom(Name), 
                             is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, 
                { Limit, MFA, Sup},[]).


start_link(Name, Limit, Sup, MFA) when is_atom(Name), 
                             is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, 
                { Limit, MFA, Sup },[]).


run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, inifinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).


% gen_server functions 

init({ Limit, MFA, Sup}) ->
    % we cannot do this because this will 
    % result in a deadlock because the server
    % process will wait for supervisor to initailize
    % and the supervisor will wait for server
    % to initalize instead we  pass a custom message
    % which we handle in handle_info so the the 
    % init in the server returns a pid
    % {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    self() ! {start_worker_supervisor,  MFA, Sup },
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

handle_info({start_worker_supervisor, MFA, Sup} , S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    {noreply, S#state{sup=Pid}};

handle_info({ 'DOWN', Ref, process, _Pid, _} , S=#state{refs=Refs}) ->
    io:format("Recieived down mesg ~n"),
    case gb_sets:is_element(Ref,Refs) of 
        true ->
            handle_down_worker(Ref,S);
        false ->
            {noreply, S}
    end;

handle_info(Msg, _State) ->
    io:format("Unknown msg :~p ~n", [ Msg ] ).

handle_call({ run, Args }, _From, S=#state{limit=N, refs=R, sup=Sup}) when N > 0 ->
    {ok, Pid } = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process,Pid),
    { reply, {ok, Pid} , S#state{ limit=N-1, refs=gb_sets:add(Ref, R)}};

handle_call({ run, _Args }, _From, _S=#state{limit = N}) when N =< 0 ->
    { reply, noalloc , _S };

handle_call({ sync, Args} , _From, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S=#state{limit = N-1, refs= gb_sets:add(Ref,R)}};

handle_call({ sync, Args} , From, S=#state{queue=Q}) ->
    {noreply, S#state{queue=queue:in({From, Args},Q)}};

handle_call( stop, _From, S=#state{}) ->
    {stop, normal, ok, S};

handle_call( _Msg, _From, State) ->
    {norply, State}.



% ASYNC logic

handle_cast({async, Args} , S=#state{limit=N, sup= Sup, refs=R}) when N>0 ->
    {ok, Pid} = supervisor:start_child(Sup,Args),
    Ref = erlang:monitor(process, Pid),
    { noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
   
handle_cast({async, Args} , S=#state{queue=Q, limit=N}) when N=<0 ->
    { noreply, S#state{ queue=queue:in(Args,Q)}};
handle_cast( _Msg, S ) ->
    { noreply, S}.

% return value is given back to  gen_server so 
% it should be in a format gen_server understands
handle_down_worker(Ref, S=#state{limit=L, sup=Sup, refs=Refs})->
    case queue:out(S#state.queue) of 
        { {value, {From, Args}} , Q} ->
            { ok, Pid } = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            gen_server:reply(From, {ok, Pid}),
            { noreply, S#state{refs=NewRefs, queue=Q}};
        {{value, Args}, Q} ->
            { ok, Pid } = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            { noreply, S#state{refs=NewRefs, queue=Q}};
        {empty,_} ->
            { noreply, S#state{ limit=L+1, refs= gb_sets:delete(Ref,Refs) }}
    end.


code_change(_OldVsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, _State)->
    ok.

