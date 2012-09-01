#!/usr/bin/env escript

-module(recursion).
-export([main/1]).


main([]) ->
    io:format("sum of list [1,2,3,4] ~p~n" , [ sum([1,2,3,4]) ]),
    io:format("length of list [1,2,3,4] ~p~n" , [ len([1,2,3,4]) ]),
    io:format("sort [4,2,8,1,257,9,1] ~p~n" , [my_quicksort([4,2,8,1,257,9,1])]).

%sum of list 
sum([])->
    0;
sum([H|T]) ->
    H + sum(T).

%tail recursive function 

sum_tail([] , Sum ) ->
    Sum ;
sum_tail([Head|Tail],Sum) ->
    sum_tail(Tail , Sum+Head).


% length of list
len([])->
    0;
len([_|T]) ->
    1+len(T).

%tail recursive funtion 
len_tail([],Len) -> Len;
len_tail([_|Tail],Len) -> len_tail(Tail,Len+1).

avg([])->
    0;
avg(List) ->
    sum(List)/len(List).


% Recursive version of quick sort : 
% here we choose the pivot element and create two lists
% larger and smaller which will then call quicksort recursively

my_quicksort([]) -> [];
my_quicksort([Pivot|Rest]) ->
    %{Small,Large} = partition(Pivot,Rest),
    {Small,Large} = partition_rec(Pivot,Rest,[],[]),
    my_quicksort(Small) ++ [Pivot] ++ my_quicksort(Large).

partition(Pivot,List) -> 
    {[ X || X <-List , X=<Pivot] , [ X || X<-List, X>Pivot ] }.

%recursive way of partitioning
partition_rec(Pivot,[],Small,Large) -> {Small,Large};
partition_rec(Pivot,[H|T],Small,Large) when H>Pivot ->
    partition_rec(Pivot,T,Small,[H|Large]);
partition_rec(Pivot,[H|T],Small,Large) when H=<Pivot ->
    partition_rec(Pivot,T,[H|Small],Large).



