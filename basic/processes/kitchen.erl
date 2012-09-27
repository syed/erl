-module(kitchen).
-export([start/0,store/2,store_with_timeout/2,take/2,take_with_timeout/2]).

% Fridge is a simple function which store 
% the Food in a list and waits for messages
% messages can be 'store' or 'take'. 


fridge(FoodList) ->
    receive 
        {From, {store, Food}} -> 
            From ! { self(),ok },
            fridge( [ Food | FoodList ] );

        {From , {take, Food}} ->

            case lists:member(Food,FoodList) of 
                true ->  
                    From ! { self() , { ok,Food} },
                    lists:delete(Food,FoodList),
                    fridge(FoodList);
                false->
                    From ! { self() , not_found },
                    fridge(FoodList)
            end;

        terminate -> 
            ok 
    end.

% Helper functions

start( ) -> 
    spawn(?MODULE,fridge,[[]]).

store(Pid,Food) ->
    Pid ! { self() , {store,Food} },
    receive 
        { _From, Msg } -> Msg
    end.

take(Pid,Food) ->
    Pid ! { self() , { take , Food } } ,
    receive 
        { _From , Msg } -> Msg
    end.


% helpers with timeout 

store_with_timeout(Pid,Food) ->
    Pid ! { self() , {store,Food} },
    receive 
        { _From, Msg } -> Msg
    after 3000 ->  %ms 
       timeout 
    end.

take_with_timeout(Pid,Food) ->
    Pid ! { self() , { take , Food } } ,
    receive 
        { _From , Msg } -> Msg
    after 3000 ->
       timeout 
    end.


