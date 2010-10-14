-module(boolean).
-export([b_not/1,b_and/2,b_or/2]).
% Write logical functions using pattern matching 



b_not(true)->
    false;

b_not(false)->
    true.

b_and(true,true) ->
    true ;
b_and(true,false) ->
    false;
b_and(false,true) ->
    false;
b_and(false,false) ->
    false.




b_or(Var1,Var2)-> 
    Var1 = Var2.
    
