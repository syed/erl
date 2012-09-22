-module(records).
-export([main/0,first_robot/0,car_factory/1]).

% records are similar to C structs 
% you can access elements like 
% rename.value1 etc 
%
% use rr(module) to import module definition
% in the shell to print it instead of displaying 
% just a tuple

-record( robot, { name , 
                type=industrial,
                hobbies,
                details=[] }).



main()->

    io:format("Record : ~p~n",[first_robot()]),
    % accessing records 
    Crusher = #robot { name="Crusher" , hobbies=["Crushing People" , "Petting Cats"] },
    io:format("Robot name ~p~n" , [Crusher#robot.name] ),

    NestedBot = #robot{details=#robot{name="erNest"}},
    io:format("Nested name ~p~n" , [(NestedBot#robot.details)#robot.name] ),

    io:format("Name is ~p~n",[get_name(Crusher)]).


% declaring a record
first_robot() ->
    #robot{ name = "Mechatron",
        type=handmade,
        details=["Moved by a small man inside"] }.

car_factory(CorpName) ->
    #robot { name = CorpName , hobbies="Building Cars"}.


% pattern matching in records.
% it is not necessary to match all parts 
% of the record just name is enough

get_name(#robot { name=Name }) ->
    Name.


