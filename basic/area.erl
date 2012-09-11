-module(area).
-export([main/1]).  
main([]) ->
    io:format("Area of squre of side 4 ~p~n",[area({square,4})]),
    io:format("Area of cirle of radi 4 ~p~n",[area({circle,4})]),
    io:format("Area of trigl of side 4 ~p~n",[area({triangle,4,4,4})]).


% We have here function clause, the correct
% function gets executed depending on where the
% input gets matched. Notice the semi-colon after
% endo of every function.

area({square,Side}) ->
    Side*Side ;
area({circle,Radius}) ->
    math:pi()*Radius*Radius;
area({triangle,A,B,C}) ->
    S = ( A + B + C ) /2,
    math:sqrt(S*(S-A)*(S-B)*(S-C));
area(_Other) ->
    {error , invalid_object}.


