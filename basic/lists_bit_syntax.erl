#!/usr/bin/env escript
-export([main/1]).

main([]) ->
    %simple list
    [1,2,3],
    % list wit lot of different elements
    [1,2,3,{numbers,[4,5,6]}, 5.34, atom],

    % list with ascii numbers --> string
    [96,98,98],
    %outputs "abc"

    % list concatnation
    [1,2,3] ++ [4,5],
    % produces [1,2,3,4,5]

    % list removal 
    [1,2,3,4,5] -- [1,2,6],
    % produces [3,4,5]

    % list constructor
    [Head|Tail] = [1,3,4,5],
    %Head = 1, Tail= [3,4,5]
    Head,Tail, % for making compiler happy

    [1|[]], % == [1]
    [2|[1|[]]], % == [2,1]

    %list comprehensions

    % normal list comprehension
    [2*N || N <- [ 1,2,3,4]],
    %produces [2,4,6,8]

    % list comprehension with predicate 
    [2*N || N <- [1,2,3,4] , N rem 2 =:= 0 ],
    % produces [4,8]
    
    % mulitple comprehensions
    [ X + Y  || X <- [1,2] , Y <- [3,4] ],
    % produces a n*m list [1+3,1+4,2+3,2+4]

    % pattern matching with list comprehension

    Weather = [{toronto, rain}, {montreal, storms}, {london, fog},  
                {paris, sun}, {boston, fog}, {vancouver, snow}],
    Foggy = [ City || {City,fog} <- Weather ],
    Foggy, % contains list of cities where fog is present 

    
    % bit syntax 

    %hex input 
    Color = 16#F09A29, % 24 bit input
    Px = <<Color:24>>, % <<240,154,41>>
    <<R:8,G:8,B:8>> = Px,
    R,G,B,

    %% also 
    P1 = Color,
    %% now 
    <<P1:8>>, % <<132>>
    <<P1:16>>, % <<45,132>>
    <<P1:24>>, % <<213,45,32>>

    % getting elemnt
    element(2,{a,b,c}),
    % gives b

    %sequence
    lists:seq(1,4).
    % [1,2,3,4]

