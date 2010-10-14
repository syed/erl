-module(sum_of_int).
-export([sum/1,sum/2]).

% Program to find the sum of numbers between 1 and N 
sum(1) -> 1;
sum(N) when N > 1 -> N + sum(N-1).


% Program to find the sum between a range [ M , N ] 

sum(M,N) when M == N ->
    N ;
sum(M,N) when N >= M  ->
    M + sum(M+1,N).





    
