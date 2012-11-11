-module(band_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1]).


start_link(Type) ->
    supervisor:start_link({local,?MODULE} , ?MODULE, Type).


% 3 types of supervisor
% 1) linient : will only restart the module which made
%       mistake. ie. one_for_one 
% 2) angry : will restart all moduels which spawned after 
%       this including this ie rest_for_one
% 3) jerk : will restart all process ie one_for_all
%

init(lenient) -> 
    % { restart_startegy, max_restarts, in_this_time }
    init({one_for_one, 3 , 60 });

init(angry) ->
    init({rest_for_one, 2 , 60 });

init(jerk) ->
    init({one_for_all, 1 , 60 });

init({RestartStrategy, MaxRestart, MaxTime})->


    % main init for the supervisor, return value shall be 
    %
    % { ok, { RestartStrategy, MaxRst, MaxTime ,  [ChildSpec] }}
    % where childspec is  list of :
    % { Id, StartFunc, Restart, Shutdown, Type, Modules }
    % 
    % *Id* : name used to identify the child internally
    % 
    % *StarFunc* : Funciton used to start the child of the form
    %   { Module, Func, Args }
    %
    %   NOTE: the StartFunc must create and *link* to 
    %   the child proc
    % 
    % *Restart* : this defines when a terminated child should be
    %   started. it can take 3 possible values
    %
    %   a) permanent : a permanant process should always be
    %       restarted.
    %   b) temporary: a temp process should never be restarted
    %       even if the restart stategy is one_for_all or 
    %       rest_for_one. 
    %   c) transient: a transient process should be restarted
    %       only if it terminates abnormally ie other than
    %       "normal" or "shutdown" 
    %
    % *Shutdown* : this defines how the child proecss should
    %   be killed. this can take 2 values
    %
    %   a) brutal_kill: this means the child will be killed
    %       using exit(ChildPid, kill). This is non maskable
    %       and process will die
    %   b) Integer : an integer value means the supervisor 
    %       will terminate the child by calling 
    %       exit(ChildPid, shutdown) and will wait for 
    %       the amount given in the Integer before calling
    %       exit(ChildPid, kill )
    %
    %       Note: it is possible to give the timeout as
    %       infinity. In that case the supervisor will
    %       wait forever. This can be potentially dange
    %       rous
    %
    % *Type* : type specifies the type of process. it can take 
    %   2 values "worker" or "supervisor" used in supervisor
    %   trees
    %
    % *Modules* : is a List object Used for some 
    %   release handling stuff when hotloading code. 
    %   put this equal to  module of the  worker
    %

    {ok, {{ RestartStrategy, MaxRestart, MaxTime},
            [ { singer,
                    {musicians, start_link, [singer, good]},
                    permanent, 1000, worker, [musicians]},

                {bass,
                    {musicians, start_link, [bass, good]},
                    temporary, 1000, worker, [musicians]},

                {drum,
                    {musicians, start_link, [drum, bad]},
                    transient, 1000, worker, [musicians]},
                {keytar, 
                    {musicians, start_link, [keytar, good]},
                    transient, 1000, worker, [musicians]}
            ]}}.
