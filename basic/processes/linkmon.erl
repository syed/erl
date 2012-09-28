-module(linkmon).

-export([main/0]).

main() -> 
    Pid = spawn(fun myproc/0),
    link(Pid), 
    receive 
        Msg -> io:format("~p~n",[Msg])
    end ,
    %creating a monitor
    % can use monitor( process, Pid ) or spawn_monitor 
    
    { Pid , Ref } = spawn_monitor(fun() -> timer:sleep(500) end),

    % we will get a message 
    % if the process dies 
    % something like 
    % {'DOWN',#Ref<0.0.0.77>,process,<0.63.0>,normal}
    
    receive
        Msg2 -> io:format("~p~n",[Msg2])
    end ,

    % we can demonitor a process if we don/t want it 

    erlang:demonitor(Ref).
    

myproc() -> 
    timer:sleep(5000),
    exit(reason).


% naming process

% judges an album , it takes a Pid of critic 
% and sends it the band and album name 
% and receives the criticism 

judge_pid ( Pid, Band, Album ) -> 
    Pid ! { self(), Band, Album },
    receive 
        { Pid, Criticism } -> Criticism 
    after 2000 ->
        timeout
    end.

% judges an album , it uses the 'critic'
% atom to send message to the critic 
% process
judge(Band,Album) ->
    critic ! { self(), Band, Album },
    Pid = whereis(ciritc), % XXX: careful here %
    receive 
        { Pid, Criticism } -> Criticism 
    after 2000 ->
        timeout
    end.

% in the previous function , the critic process can crash 
% and it is possible that the critic atom is left with 
% invalid Pid, we have to make sure that we are safe. 
% We can do this by sending our ref along with the 
% other args to critic

judge_with_ref(Band,Album) ->
    Ref = make_ref(),
    critic ! { self(), Ref,Band, Album },
    receive 
        { Pid,Ref,Criticism } -> Criticism 
    after 2000 ->
        timeout
    end.



% the critic process which does the
% actual work

critic() -> 
    receive  
            {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
                From ! {self(), "They are great!"};
            {From, {"System of a Downtime", "Memoize"}} ->
                From ! {self(), "They're not Johnny Crash but they're good."};
            {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
                From ! {self(), "Simply incredible."};
            {From, {_Band, _Album}} ->
                From ! {self(), "They are terrible!"}
    end,
    critic().

%same critic but which handles Refrences. This is good as
%we can ensure there is no race condition 

critic_with_ref() -> 
    receive  
            {From, Ref,{"Rage Against the Turing Machine", "Unit Testify"}} ->
                From ! {self(), Ref,"They are great!"};
            {From, Ref, {"System of a Downtime", "Memoize"}} ->
                From ! {self(), Ref,"They're not Johnny Crash but they're good."};
            {From, Ref,{"Johnny Crash", "The Token Ring of Fire"}} ->
                From ! {self(), Ref,"Simply incredible."};
            {From, Ref,{_Band, _Album}} ->
                From ! {self(), Ref,"They are terrible!"}
    end,
    critic_with_ref().



% starting critic  just a single function.
% if ciritc dies, we have to find it out 
% ourselves and call this function again

start_ciritic_single() -> 
    spawn( ?MODULE , critic, [] ).

% restarter for critic process. If 
% critic dies, it will restart it . 
% The restarter also registers the
% critic to an atom 'critic' which
% can be used from other functions.

restarter() -> 
   process_flag(trap_exit, true ), % we have to do this because we want to 
                                    % restart the process, if we don't do this
                                    % we will also die if child dies ( because we are linking )

   Pid = spawn_link(?MODULE,critic,[]),
   register(critic,Pid), % we are assigning a Pid to an atom which can be acceced from anywhere 
   receive 
    { 'EXIT' , Pid, normal } -> ok; % critic exited normally ( planned ) 
    { 'EXIT' , Pid, shutdown } -> ok; % critic exited by sending shutdown command  ( planned ) 
    { 'EXIT' , Pid, _ } -> restarter() % anything else is a crash, we should restart critic
   end. 
