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

from the shell comple like so ::      

    $erl
    >c(hello).
    >hello:start().


