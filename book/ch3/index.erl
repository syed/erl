-module(index).
-export([open_file/1]).

% opens a file and passes it to raw_document
open_file(Filename) ->
    {ok,Fd} = file:open(Filename,[read]),
    RawList = raw_document(Fd),
    ListOfWords = document(RawList),
    index(ListOfWords,1,[]).


% reads a line from a file and stores it in a list 
raw_document(Fd) ->
    Ret= file:read_line(Fd),
    
    case Ret of 
	{ok,Data} ->
	    [Data|raw_document(Fd)];
	eof ->
	    [];
	{error,Reason} ->
	    io:format("Error : ~p" , Reason)
    end.

% reads a list of lines and returns a  list of words 

document([])->
    [];
document([H|T]) ->
    % Head is a string , break into words 
    Words = string:tokens(H," \n"),
    Words ++ document(T).

% Reads list of words and returns a tuple with word and occurences 

index([],_,Tup)->
    Tup;

index([W|T],Count,Tup) ->
    T2 = find_and_insert(W,Count,Tup),
    %io:format("find_and_insert gave ~p ~n:" ,[T2]),
    index(T,Count+1,T2).


find_and_insert(W,Count,[])->
    % Did not find in the tuple , Insert it 
    [{W, [Count]}];

find_and_insert(W,Count,[{W1,L1} |T]) when W1 == W ->
    T2 = [{W,[Count|L1]} | T ],
    T2;

find_and_insert(W,Count,[H|T]) ->
    [H| find_and_insert(W,Count,T)].


	    


    
    

    
