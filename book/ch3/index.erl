-module(index).
-export([open_file/1]).

% opens a file and passes it to raw_document
open_file(Filename) ->
    {ok,Fd} = file:open(Filename,[read]),
    RawList = raw_document(Fd),
    ListOfWords = document(RawList).



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


    
    

    
