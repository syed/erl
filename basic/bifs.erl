-module(bifs).
-export([obj_access/0,get_stuff/0,format_print/0]).


% Common bifs 

obj_access()->
    List = [1,2,3,4],
    Head = hd(List),
    io:format("Head:~p~n",[Head]),
    Tail = tl(List),
    io:format("Tail:~p~n",[Tail]),
    Len = length(List),
    io:format("Len:~p~n",[Len]),
    Tuple = {1,2,3,4,5},
    Size = tuple_size(Tuple),
    io:format("TupleSize:~p~n",[Size]),
    Ele = element(2,Tuple),
    Tuple2=setelement(3,Tuple,three),
    io:format("NewTuple:~p~n",[Tuple2]),
    erlang:append_element(Tuple,6).
%   io:format(Tuple).

get_stuff() ->
    %get a line from stdin 
    Line = io:get_line("EnterLine>"),
    io:format(Line),
    % get some chars 
    Chars = io:get_chars("EnterChars>" , 2 ),
    io:format(Chars),
    % Most important read which reads erlang term from stdin
    io:read("ok,then>>").
    % can input a list tuple atom etc will return a tuplle { ok , <term> }

        
format_print() ->
    List = [1,2,3,4],
    Tuple = {abc,def,ghi},
    Int = 1,
    Float  = 2.34,
    Str = "teststring",
    % ~p - pretty printing 
    % ~w - standard syntax like you would see on the shell 
    % ~W ~P - take an extra arg specifing the depth to print 
    %  ~n - newline 
    io:format("List : ~P , Tuple : ~p , Int : ~p , Float ~p , Str : ~p " ,[List , 3 , Tuple , Int , Float , Str ] ).
