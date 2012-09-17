-module(errors_exceptions).
-export([main/0]).


main()->
    % Errors, Throws, Exits
    %
    % Erorrs : Stop execution, include stack trace 
    
    %erlang:error(custom_error),

    % Exits: Stop execution, No stack trace
    % two kinds internal and external 
    % internal exit just quits the process extrnal 
    % exit for killing other procesa
    %

    %erlang:exit(something_went_wrong). 

    % throws are errors which programmer
    % is expected to handle this is useful
    % for deep recursion where returning
    % on error is fast

    %throw(permission_denied).


    % try catch block 
    %
    % try Expression of 
    %   SuccessPattern1 [Guards] ->
    %       Expression1;
    %   SuccessPattern2 [Guards] ->
    %       Expression2;
    % catch
    %   TypeOfError:ExceptionPattern1 ->  %%% Notice No guards
    %       Expression3;
    %   TypeOfError:ExceptionPattern2 ->
    %       Expression4;
    % end
    %

    F = (fun error_throwing_function/1),
    try_catch_test(F,1),
    try_catch_test(F,2),
    try_catch_test(F,3),

    % we can have an after which acts like a "finally" block 
    % which gets executed everytime 

    % try Expression of
    %   SuccesPatten1 [Guards] ->
    % catch 
    %   Errorpattern ->
    % after 
    %   Code that runs everytime 
    % end.
    %

    % catch keyword : it is used to catch errors as well as result
    % genrally used with case ... of 
    %

    io:format("divide 8,3 with catch ~p~n" ,[catch_keyword_divide(8,3)]),
    io:format("divide 6,0 with catch ~p~n" ,[catch_keyword_divide(6,0)]),

    retval.


try_catch_test(F,Case) ->

    Val = try F(Case) of 
                _ -> { ok }
    catch 
        ThrowData -> { throw,ThrowData};  %default is assumed to be throw
        error:ErrorData -> { error, ErrorData };
        exit:ExitData -> { exit , ExitData }

    end,
    io:format("val : ~p~n",[Val]).

% a function which throws different kinds of errors 
error_throwing_function(1) -> throw(permission_denied);
error_throwing_function(2) -> erlang:error(invalid_username);
error_throwing_function(3) -> exit(internal_error);
error_throwing_function(_) -> undef.

% function which uses the "catch" keyword 

catch_keyword_divide(X,Y) ->
    case catch X/Y of  
            { 'EXIT' , ErrorData } -> ErrorData;
            N -> N
    end.


% example of a tree lookup program which returns
% true if found to its parent which intern does
% the same until it reaches the root

has_val(_,{node,nil}) ->
    false;
has_val(Val,{node,_,Val,_,_}) ->
    true;
has_val(Val,{node,_,_,Left,Right}) ->
    case has_val(Val,Left) of 
        true ->true;
        false -> has_val(Val,Right)
    end.
    

% here we implement a tree search where we use
% throws to quickly get out of a deep recursion to 
% retun the value as soon as we find it

has_val_throw(Val,Tree) ->
    try has_val1(Val,Tree) of 
        false -> false
    catch 
        true -> true
    end.

has_val1(_,{node,nil})->
    false;

has_val1(Val,{node,_,Val,_,_})->
    throw(true);
has_val1(Val,{node,_,_,Left,Right}) ->
    has_val1(Val,Left),
    has_val(Val,Right).



