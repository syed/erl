-module(curling_scoreboard_hw).
-export([add_point/1, next_round/0, set_teams/2, reset_board/0]).

% these functions will be called when the event
% handler receives the event 
set_teams(TeamA,TeamB) ->
    io:format("Team ~s vs. Team ~s~n",[TeamA,TeamB]).

next_round()->
    io:format("Round over~n").

add_point(Team) ->
    io:format("Increased score of ~s by 1~n",[Team]).

reset_board()->
    io:format("Scores reset to 0~n").



