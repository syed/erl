-module(recursion).
-export([sum/1,try_return/1]).
%sum of list 
sum([])->
    0;
sum([H|T]) ->
    H + sum(T).


len([])->
    0;
len([_|T]) ->
    1+len(T).

avg([])->
    0;
avg(List) ->
    sum(List)/len(List).

%tail recursive function 

sum_acc([] , Sum ) ->
    Sum ;
sum_acc([Head|Tail],Sum) ->
    sum_acc(Tail , Sum+Head).

% Handling runtime errors 

sample_try_catch()->
    X=2,
    try (X=3) of 
        Val -> { normal , Val }
    catch 
        _:_ -> 43 
    end.

return_error(X) when X < 0 ->
    throw({'EXIT' ,{badarith ,[ { exception,return_error,1} , {erl_eval , do_apply,5},{shell,expers,6} ] } } );

return_error(X) when X == 0  ->
    1/X;
return_error(X) when X > 0 ->
    {'EXIT' , { badairth , [ exit_status , 1] } } .


try_return(X) when is_integer(X) ->
    try return_error(X) of 
        Val -> { normal , Val }
    catch 
        exit:Reason ->
	    {exit , Reason };
	throw:Throw ->
            {throw , Throw };
	error:Error  ->
            {error,Error}
    end.


