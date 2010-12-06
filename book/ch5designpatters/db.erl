-module(db).
-export([new/0,destroy/1,write/3,delete/2,read/2,match/2]).

% Program that implements a simple DB using lists and tuples 


% Creates an empty DB
new()->
    [].


% For now no implementation 
destroy(Db)->
    ok.

% Write Value into DB . Does not handle collision 
write(Key,Value,Db) ->
    T={Key,Value},
    [T|Db].


% Read from a DB
read(Key,[])->
    {error,instance};
read(Key,[{K1,V1}|T]) when Key == K1  ->
    {ok,V1};
read(Key,[{K1,V1}|T]) when Key /= K1  ->
     read(Key,T).

% Delete from a DB 

delete(Key,[]) ->
    [];
delete(Key,[{K1,V1}|T]) when Key == K1 ->
    [delete(Key,T)];
delete(Key,[H|T]) ->
    [H|delete(Key,T)].



% Returns a list of Keys for a given Value 
match(Element,[])->
    [];
match(Element,[{K1,V1}|T]) when V1 == Element ->
    [K1|match(Element,T)];
match(Element,[H|T]) ->
    match(Element,T).
