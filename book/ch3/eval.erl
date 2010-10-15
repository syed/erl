-module(eval).
-export(parser/1).

% A program to read a fully bracketed expression and parse it 
% into erlang representation 


% ((2+3)-4) to {minus,{plus,{num,2},{num,3}},{num,4}}

parser(Expr) ->
    
