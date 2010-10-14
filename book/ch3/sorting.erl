-module(sorting).
-export([quicksort/1,mergesort/1]).

% Quicksort main logic 
quicksort([])->
    [];
quicksort([H|T]) ->
    L1 = split_less(T,H),
    L2 = split_more(T,H),
    L11=quicksort(L1),
    L22=quicksort(L2),
    L3 = list_merge(L11,[H|L22]),
    L3.


% Return a list of values less than the given value 
split_less([],_)->
    [];
split_less([H1|T1],H) when H1 =< H   ->
    [H1|split_less(T1,H)];
split_less([_|T1],H) ->
    split_less(T1,H).

% Return a list of values greater than given value 
split_more([],_)->
    [];
split_more([H1|T1],H) when H1 > H   ->
    [H1|split_more(T1,H)];
split_more([_|T1],H) ->
    split_more(T1,H).

% merge  lists along with the pivot element 

list_merge([],L2)->
    L2;

list_merge([H|T],L2) ->
    [H|list_merge(T,L2)].


mergesort(L)->
    [].
