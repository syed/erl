-module(basics).
-export([stuff/0]).

stuff() ->
    A=1,
    B=2,

    % A=A+1
    % This is wrong cannot modify A 

    L=[A|[2,3]],
    io:format(L).
