-module(higher_order_functions).
-export([main/1,inc/1,dec/1,even_check/1]). %exporting inc,dec,even_check to make compiler happy


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
    Alarm(),

    % Using anonymous functions to work around cases where
    % we have to pass multiple arguments to a function but are 
    % restricted for example in map, we will pass the function 
    % which gets executed on every element but we cannot give 
    % it extra arguments 
    %

    io:format("funtion math:pow(2,5) : ~p~n" , [math:pow(2,5)]),
    
    Base = 2,
    PowerOfTwo = fun(X) -> math:pow(Base,X) end, % notice that base is available here 

    io:format("Power of 2 of [1,2,3,4] is ~p~n",[ map(PowerOfTwo,[1,2,3,4])]),

    io:format("2 is even is ~p~n",[even_check(2)]),

    % example of using filter , pick out the even elements in a list

    io:format("Even elements in [3,1,6,2,5,8] ~p ~n",[ filter(fun higher_order_functions:even_check/1 , [3,1,6,2,5,8]) ] ).


%example fuction is increment  and decrement
% we will use them in main 

inc(X) -> X+1.
dec(X) -> X-1.


% map takes a function and list it applies
% that function to each elemement and returns
% a new list 

map( _, [] ) -> [];
map( F , [H|T] ) ->  [F(H)|map(F,T)].


% keep even  elements in a list
% we will use filter abstraction to implement
% this


even_check(Element) when Element rem 2 ==  0 ->
    true;
even_check(_) ->
    false.



% Filter abstraction , this simple abstraction allows to take a list 
% apply a function to each element of the list , if they succeed ,
% it will keep it else it will discard it.

filter(TestFunction,ListOfElements) -> lists:reverse(filter(TestFunction,ListOfElements,[])).

filter(_TestFunction,[],FinalList) -> FinalList;
filter(TestFunction,[H|T],FinalList) ->
    case TestFunction(H) of 
            true -> 
                filter(TestFunction,T,[H|FinalList]);
            false ->
                filter(TestFunction,T,FinalList)
    end.


