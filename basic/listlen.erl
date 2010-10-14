-module(listlen).
-export([listlen/1,listlen_fun/1,checkeven/1,preferred/1,factorial/1,even/1]).
% Functions to calculate list length using case and fucntion
% pattern matching 

listlen(Y) ->
    case Y of 
	[] -> 0 ;
	[_|Xs] -> 1 + listlen(Xs)
    end.


listlen_fun([])->
    0;
listlen_fun([_|Xs] ) ->
    1+listlen_fun(Xs).

% Preferred way to assign values from a case statement 

preferred(X) ->
    Y = case X of 
	    one -> 12;
	    _ -> 196
	end ,	     
    X+Y.

% If construct 

checkeven(X)->
    if 
	X rem 2 == 0 ->
	    even;
	X rem 2 == 1  ->
	    odd
    end.


% Guards
% Guards do not allow user defined functions to be called inside them 
% guards seperated by comma are conjunction(and) and seperated by semicolon are 
% disjunction (or)


factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N*factorial(N-1).
     
even(Int) when Int rem 2 == 0 ->
    true ;
even(Int) when Int rem 2 == 1  ->
    false.

