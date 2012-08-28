-module(types).

-export([main/1]).

main([]) ->
    io:format("Test").



type_conversions() ->
    % some types are implicitly converted like 
    io:format("Adding 4 (int) and 3.7 ( float ) is OK ~p~n" , [4+3.7]),
    % for string to int conversion we have 
    % note that this won't work for "52.23" a float string
    io:format("String "52" to in ~p~n" , [list_to_integer("52")]),
    % for float string we have to use different function 
    io:format("String "52.23" to in ~p~n" , [list_to_float("52.23")]),
    % similarly we have  atom_to_list , list_to_atom
    % list_to_bitstring , bitstring_to_list.  and lot more 
    % see http://learnyousomeerlang.com/types-or-lack-thereof#dynamite-strong-typing




