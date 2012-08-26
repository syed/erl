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

======================
Common syntax mistakes
======================

* Not using comma in case of multi line statement::

   X = 32 <-- missing comma
   Y = 38 

   compiles to give the followoing error::

   lists_bit_syntax.erl:7: syntax error before: Y

==============================
Useful libraries and man pages
==============================

`io:format <http://erlang.org/doc/man/io.html#format-3>`_  man page 


=========================
Resources and other stuff
=========================

* books 

    - `learn you some erlang <http://learnyousomeerlang.com>`_ book
    - `Erlang programming <http://en.wikibooks.org/wiki/Erlang_Programming>`_  on Wikibooks
    - `Erlang programming <http://www.erlangprogramming.org/>`_ by O'Reilly 

* Other git repos for the book excersises

    - http://github.com/ngpestelos/misc_erlang
    - http://github.com/caioariede/erlang-programming-book

* Google Group for discussion  

    - http://groups.google.com/group/erlang-programming-book




