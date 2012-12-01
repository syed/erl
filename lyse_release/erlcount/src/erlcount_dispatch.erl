-module(erlcount_dispatch).
-behaviour(gen_fsm).

-export([start_link/0, complete/4]).
-export([init/1, dispatching/2, listening/2, 
        handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-define(POOL, erlcount).
-record(data, { regex=[], refs=[]}).


start_link()->
    gen_fsm:start_link(?MODULE,[],[]).

complete(Pid, Regex, Ref, Count) ->
    gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).


% FSM functions
% when we return from init , we will be in 'dispatching' state
init([]) ->
    {ok, Re} = application:get_env(regex),
    {ok, Dir} = application:get_env(directory),
    {ok, MaxFiles} = application:get_env(max_files),
    ppool:start_pool(?POOL, MaxFiles, {erlcount_worker, start_link, 
            [] }),
    case lists:all(fun valid_regex/1, Re) of 
        true ->
            self() ! {start, Dir},
            {ok, dispatching, #data{regex=[{R,0} || R <- Re]}};
        false ->
            {stop, invalid_regex}
    end.

% we are in 'dispatching' state now. From the find_erl
% function we can get 'done' or '{continue, File, ContinueFunc}'
% if we are done, we will output the result stored in the state
% else we will push an async job to ppool to start a worker

handle_info({start,Dir}, State, Data ) ->
    gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
    {next_state, State, Data}.

dispatching({continue, File, Continuation}, Data=#data{regex=Re, refs=Refs}) ->
    F= fun({Regex, _Count}, NewRefs) ->
            Ref = make_ref(),
            ppool:async_queue(?POOL,[self(),Ref, File, Regex]),
            [Ref| NewRefs]
    end,
    % Acc = lists:foldl( Func, Acc0, List)
    NewRefs = lists:foldl(F, Refs, Re),
    gen_fsm:send_event(self(), Continuation()),
    { next_state, dispatching, Data#data{refs = NewRefs}};

dispatching(done, Data) ->
    % see the comment in lyse as to why we call
    % listening directly instead of moving to 'listening' state
    listening(done,Data).



listening(done, #data{ regex=Re, refs=[] })-> % all done
    [io:format("Regex ~s has ~p results~n",[R,C]) || {R,C} <- Re ],
    {stop, normal, done};

listening(done,Data) -> % not fully done 
    { next_state, listening, Data}.

handle_event({complete, Regex, Ref, Count}, State, Data=#data{regex=Re, refs=Refs}) ->
    { Regex, OldCount} = lists:keyfind(Regex, 1, Re),
    NewRe = lists:keyreplace(Regex, 1 , Re, {Regex, OldCount+Count}),
    NewData = Data#data{regex=NewRe, refs=Refs--[Ref]},
    case State of 
        dispatching->
            {next_state, dispatching, NewData};
        listening ->
            listening(done, NewData)
    end.

handle_sync_event(Event, _From, State, Data)->
    io:format("Unexpected event : ~p in ~p~n", [Event, State]),
    { next_state, State, Data}.

terminate(_Reason, _State, _Data)->
    ok.

code_change(_OldVsn, State, Data, _Extra)->
    {ok, State, Data}.



%% Helper functions 

valid_regex(Re) -> 
    try re:run("", Re) of 
        _ -> true
    catch 
       error:badar ->false
    end.
                                    








