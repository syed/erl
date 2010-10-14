-module(sideeffects).
-export([printnum/1,printeven/1]).


% Program to print numbers from 1 to N 
printnum(1) ->
    io:format("Number: 1 ~n");
printnum(N) ->
    printnum(N-1),
    io:format("Number: ~p ~n" , [N]).

% Program to print even numbers between 2 to N 


printeven(2) ->
    io:format("Number: 2~n");
printeven(N) when N rem 2 == 0  ->
    printeven(N-2),
    io:format("Number: ~p~n" , [N]);
printeven(N) when N rem 2 == 1  ->
    printeven(N-1).




