-module(sorting).
-export([quicksort/1,mergesort/1,qsort/1]).

% Quicksort main logic ( UGLY !!!!!!!! )
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

% merge sort 

mergesort([])->
    [];

mergesort([H|[]]) -> 
    [H];	 
	     
mergesort(L) ->
    { L1 , L2 } = split(L),
    io:format("{ L1 , L2 } = { ~p , ~p } ~n" , [L1,L2]),
    merge(mergesort(L1) , mergesort(L2) ).


% split list into 2 parts 

split(L) ->
    split(L , {[] ,[]}).


% split does not necessarly have to be 1st half and second half
% you can split by alternatively putting elements in two lists 

split([] , { X , Y } ) ->
    {X , Y};
split([H|T] ,{ X , Y }) ->
    split(T , {Y , append(X , [H])} ).

append(A,B) ->
    A ++ B .



merge([],L2) ->
    L2;
merge(L1,[]) ->
    L1;

merge([H1|T1],[H2|T2]) when H1 >=  H2  ->
    [H2|merge([H1|T1],T2)];
merge([H1|T1],[H2|T2]) when H2 > H1 ->
    [H1|merge(T1,[H2|T2])].



% A very short implementation of quick sort ( copied from wikipedia :P) 

qsort([]) ->
    [];

qsort([Pivot|T]) ->
    qsort([Front|| Front <- T, Front < Pivot])
    ++ [Pivot] ++ 
    qsort([Back||Back <- T , Back >= Pivot ]).



