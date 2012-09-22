
-module(road).

-export([main/0]).

main()->
   File="road.txt",
   Input = parse_input(File),
   io:format("got list ~p~n",[Input]).


parse_input(File) ->
    {ok,Bin} = file:read_file(File),
    S = string:tokens(binary_to_list(Bin)," \t\n"),
    L = [ list_to_integer(X) || X<-S ],
    io:format("Path: ~p~n",[optimal_path(group_vals(L,[]))]).
 
group_vals([],Acc) -> lists:reverse(Acc);
group_vals([A,B,X|Rest] , Acc ) ->
    group_vals(Rest,[{A,B,X} | Acc] ).



optimal_path(Map) ->
    {A,B} = lists:foldl(fun shortest_step/2, { {0,[]} , {0,[]} }, Map),
    {_Dist,Path} = case element(1,A) >= element(1,B)  of
       true -> A;
       _ -> B
    end,
    lists:reverse(Path).



shortest_step( {A,B,X} ,{ { DistA,PathA} , {DistB,PathB} } ) ->
    OptA1 = { DistA + A , [ {a,A} | PathA] },
    OptA2 = { DistB + B + X , [ {x,X} , {b,B} | PathA] },
    OptB1 = { DistB + B , [ {b,B}| PathB ] },
    OptB2 = { DistA + A + X, [ {x,X} , {a,A} | PathB ] },
    { erlang:min(OptA1,OptA2) , erlang:min(OptB1,OptB2) }.





