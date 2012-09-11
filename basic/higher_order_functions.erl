-module(higher_order_functions).
-export([main/1,inc/1,dec/1]). %exporting inc,dec to make compiler happy


main([]) ->
    % we are applying a map to inc and dec on a list
    io:format("incremented [1,2,3,4] ~p~n", [map(fun higher_order_functions:inc/1 , [1,2,3,4])]),
    io:format("decremented [1,2,3,4] ~p~n", [map(fun higher_order_functions:dec/1 , [1,2,3,4])]),
    %io:fomrat("decremented [1,2,3,4] ~p~n", map(fun higher_order_functions:dec/1 , [1,2,3,4])).
    %% Anonymous functions %%
    % fun(Args) ->
    %       Expr1,Expr2....;
    %   (Args2) -> 
    %       Expr1,Expr2....;
    %   (Args3) -> 
    %       Expr1,Expr2....
    % end
    %
    io:format("incremented [1,2,3,4] ( anon func ) ~p~n", [map(fun(X) -> X+1 end  , [1,2,3,4])]),

    % Closures , access variables of parent function 
    %
    PrepareAlarm = fun(Place) -> 
            io:format("Putting alarm in ~p this function \
                will return a function which when called \
                will trigger the alarm ~n",[Place]),
                fun() ->
                        io:format("Alaram tripped in ~p~n",[Place]) % notice how Place is accessable here
                end
        end,

    Alarm = PrepareAlarm("Study"),
    Alarm().



%example fuction is increment  and decrement
% we will use them in main 

inc(X) -> X+1.
dec(X) -> X-1.


% map takes a function and list it applies
% that function to each elemement and returns
% a new list 

map( _, [] ) -> [];
map( F , [H|T] ) ->  [F(H)|map(F,T)].



