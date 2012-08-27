-module(area).
-export([area/1]).

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
area(Other) ->
    {error , invalid_object}.


