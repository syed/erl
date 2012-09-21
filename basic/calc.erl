-module(calc).

-export([rpn/1,main/0]).

main() ->
        io:format("rpn(2 3 +) ~p~n",[rpn("2 3 +")]).

%        87 = rpn("90 3 -"),
%        -4 = rpn("10 4 3 + 2 * -"),
%        -2.0 = rpn("10 4 3 + 2 * - 2 /" ).




% Simple module which is a reverse
% polish notation based calculator

rpn(L) -> 
    [Res] = lists:foldl(fun rpn/2,[],string:tokens(L," ")),
    Res.


rpn("+" ,[N1,N2|S]) -> io:format("N1,N2, S ~p ~p ~p ~n",[N1,N2,S]),[N1+N2 | S];
rpn("-" ,[N1,N2|S]) -> [N1-N2 | S];
rpn("*" ,[N1,N2|S]) -> [N1*N2 | S];
rpn("/" ,[N1,N2|S]) -> [N1/N2 | S];
rpn("^" ,[N1,N2|S]) -> [math:pow(N2,N1) | S];
rpn(X,Stack) -> [read_num(X)|Stack].


% read_num converts string to float/int 

read_num(X)->
    case string:to_float(X) of 
        {error,no_float} -> list_to_integer(X);
        {F,_} -> F
    end.


