-module(list_create).
-export([create/1,reverse_create/1,reverse/2]).

% Program to genrate a list from 1 ... N 

create(N) ->
    reverse(reverse_create(N),[]).


% Program to generate a reversed list from N ..... 1 


reverse_create(1) ->
    [1];
reverse_create(N) when N > 1 ->
    [N | reverse_create(N-1)].

% Program to reverse a list 

reverse([],Acc) ->
    Acc;
reverse([H|T],Acc) ->
    reverse(T,[H|Acc]).
    

