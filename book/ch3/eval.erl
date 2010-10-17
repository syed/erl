-module(eval).
-export([parser/1,is_digit/1]).

% A program to read a fully bracketed expression and parse it 
% into erlang representation 


% ((2+3)-4) to {minus,{plus,{num,2},{num,3}},{num,4}}


parser([]) ->
    {};
parser([H|T]) ->
    if 
	is_open_bracket(H) -> io:format("Open bracket~n");
	  is_close_bracket(H) -> io:format("Close bracket~n")
    end.

	    




% Helper functions 

is_open_bracket($() ->
    true;
is_open_bracket(V)->
    false.

is_close_bracket($))->
    true;
is_close_bracket(V) ->
    false.

is_digit(V) when V >= $0 ,  V =< $9 ->
    true ;
is_digit(V ) -> false.


