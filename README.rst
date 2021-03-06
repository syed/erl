=======================
Running erlang programs
=======================

For simple programs, use escript 

Here is a simple hello world in escript::

    #!/usr/bin/env escript
    -export([main/1]).
    
    main([]) -> io:format("Hello, World!~n").

for complex programs however, compile the standard way 

source code ::

    -module(hello).
    -export([start/0]).

    start() ->
      io:format("Hello, World!").

from the shell comple & run like so ::      

    $erl
    >c(hello).
    >hello:start().

to compile from a Emakefile do ::

    $ cd /path/to/emakefile
    $ erl -make

======================
Common syntax mistakes
======================

* Not using comma in case of multi line statement::

   X = 32 <-- missing comma
   Y = 38 

   compiles to give the followoing error::

   lists_bit_syntax.erl:7: syntax error before: Y
   
* Forgetting that io:format takes a list to substitute values.::

    io:format("Value of X and Y are ~p , ~p" , X,Y) % Wrong, X,Y should be in a list
    io:format("Value of X and Y are ~p , ~p" , [X,Y]) % Correct

* Not closing function clauses with semi-colon ( last one will close with a ".")::

    valid_time({Y,M,D),{H,Min,S})->
        io_format("Good") . <-- There should be a semi-colon here instead of dot as it is part of clause
    valid_time(_) ->
        io:format("Invalid input"). <- last function in the clause ends with dot 
    
* look out for less than equals and greater than equals::

    <= wrong =< right
    >= right => wrong 

* forgetting semi-colon in  both `if` and `case ... of` clauses and adding semi-clon in the last clause::

    if X == vala ->
        Res = call_some_function(X),
        Res; <--- this semi-colon should be there for all-but-last if clause
    if X == valb -> 
        Res = call_some_other_function(X),
        Res <--- last clause should not have anything. It is the end that completes the if block
    end,

    % case can let us do pattern matching 
    case X of ->
        vala ->
            Res = call_some_function(X),
            Res;
        valb ->
            Res = call_some_other_function(X),
            Res
    end.  
       
* ending the last statement of an anonymous funtion with a period. In a normal fuction last statement is followed by a period::


      % Normal function 
      do_something(X) ->
        io:format("Printing value of X ~p" ,[ X ] ),
        io:format("Doing somethiing~n"). % <-------- notice the period here

      DoSomething = fun(A) -> 
        io:format("Printing value of X ~p" ,[ X ] ),
        io:format("Doing somethiing~n") %<--------  no period is required 
      end % <------ , or . depending on where it is 

* putting -> after `receive` or leaving -> after `after` ::

    % this is a bit weird but have to live with the syntax
    % there should be no '->' after the receive and there should be
    % a '->' after the 'after'
    receive 
      { From, Msg } -> do_something(Msg)
    after 2000 -> 
      timeout
    end,

* be very careful with records ::

    % observe how records are used in the function head and the return 
    % see the '=' sign 
    -record( state, { name="testing", 
                        value=0,
                        flags }.
    increase_val( S=#state{ value=V, name=Name}, IncVal) ->        % notice the '=' here
        %increases the value of state by IncVal amount
        io:format("increasing value of ~p by ~p~n",[Name,IncVal]),
        S#state{ value=V+Inc}.                                     % no '=' in the return value


  

==============================
Useful libraries and man pages
==============================

* `io:format <http://erlang.org/doc/man/io.html#format-3>`_  man page 
* `Coding standards <http://www.erlang.se/doc/programming_rules.shtml>`_ for erlang
* `list of functions/bifs <http://learnyousomeerlang.com/types-or-lack-thereof#dynamite-strong-typing>`_ allowed in guards
* Erlang `design principles http://www.erlang.org/doc/design_principles/applications.html#id71171>`_



=========================
Resources and other stuff
=========================

* books 

    - `learn you some erlang <http://learnyousomeerlang.com>`_ book
    - `Erlang programming <http://en.wikibooks.org/wiki/Erlang_Programming>`_  on Wikibooks
    - `Erlang programming book <http://www.erlangprogramming.org/>`_ by O'Reilly 

* Other git repos for the book excersises

    - http://github.com/ngpestelos/misc_erlang
    - http://github.com/caioariede/erlang-programming-book

* Google Group for discussion  

    - http://groups.google.com/group/erlang-programming-book
