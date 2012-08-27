-module(functions).

-export([main/1]).

main([])->
    io:format("Sample functions~n"),
    List=[1,2,3,4,5],
    io:format("Head of list ~p , 2nd elment of list ~p~n" , [head(List) , second(List)]),
    valid_time( { {2012,09,10} , {23,13,53} } ),
    io:format("13 old enough ? ~p , 20 old enough ? ~p~n", [is_old_enough(13) , is_old_enough(20)] ),
    io:format("13 right age ? ~p , 20 right age ? ~p~n", [right_age(13) , right_age(20)] ),
    io:format("13 wrong age ? ~p , 20 wrong age ? ~p~n", [wrong_age(13) , wrong_age(20)] ),
    io:format("check value 1  ~p , check value 0  ~p, check value 2  ~p~n", [check_fine(1) , check_fine(0), check_fine(2)] ).

    

%---------------------------------------
% Function : head/1
% Purpose : behaves like the standard hd
%           function which gives the head
%           of a list.
% Args : A list of elments or empty list
% Return value : head of the list 
%----------------------------------------

head([H|_]) -> H.

% returns 2nd element from list
second([_,S| _]) -> S.


%---------------------------------------
% Function : valid_time/1
% Purpose : prints date and time if input  
%           given is valid. Otherwise will
%           print error.
%
% Args : A tuple of type {{Y,M,D},{H,Min,S}}
% Return value : none, output is printed
%----------------------------------------

% notice here that the input date will pattern match
% against both Date and {Y,M,D} and assigned to both ( in case of match )

valid_time({ Date = { Y,M,D } , Time = {H,Min,S} } ) ->  
    io:format("Date : ~p/~p/~p time : ~p:~p:~p~n",[Y,M,D,H,Min,S]);
valid_time(_) ->
    io:format("Error, invalid input~n").

% example using guards where we require more than 
% pattern matching like comparing range etc.In this 
% example we are checking if age is greater than 16 
% or not 

is_old_enough(Age) when Age =<  16 ->
    false;
is_old_enough(_) ->
    true.


%more than one guard , here comma acts as logical AND
right_age(Age) when Age >=16,Age=<100 ->
    true;
right_age(_) ->
    false.

% semi-colon acts as OR 
wrong_age(Age) when Age < 16 ; Age > 100 ->
    true;
wrong_age(_) ->
    false.

% note here that we can replace "," with andalso and ";" with orelse.
% the main difference is that we can nest andalso, orelse
% but we cannot do tihat with comma and semi colon 
% (A.B);C is not allowed but ((A andalso B ) orelse C) will work`


% erlang if's. 
% They are similar to guards , is and and ; is or 

check_fine(Value) ->
    Ret = if Value =:= 1 ->  % we can assign the return of this if clause
            fine; %returns this atom  
       Value =:= 0 -> 
            not_fine;
       true -> % catch all. Acts like else     
           invalid_value
    end,
    Ret.

% case of .... this is more full fleddged way of conditianlly executing code
% in a function . We can use complex pattern matching here and can have guards
% in each clause 

insert(X,[]) ->
    X;
insert(X,Set) ->
    case lists:member(X,Set) of 
        true -> Set;
        false -> [X|Set]
    end.


% exmaple with guards 
beach(Temperature) ->
    case Temperature of 
        {celsius,N} when N>=20, N =< 45 -> 
            favourable;
        {kelvin, N} when N >= 293, N =< 318 ->
            scientifically_favorable;
        _ ->  %catch all
            invalid

    end.    
