-module(aoc_problem_5).
-export([solve/2]).

solve(Problem, Example) ->
    [DrawingRaw, CommandsRaw] = aoc_input:split_by_blanks(Problem, Example),
    Drawing = parse_drawing(DrawingRaw),
    %io:format("Drawing=~p~n", [Drawing]),
    Commands = parse_commands(CommandsRaw),
    %io:format("Commands=~p~n", [Commands]),
    Part1End = lists:foldl(fun execute_command/2, Drawing, Commands),
    io:format("Part 1: ~s~n", [read_tops(Part1End)]),
    Part2 = lists:foldl(fun execute_command_2/2, Drawing, Commands),
    io:format("Part 2: ~s~n", [read_tops(Part2)]).

parse_drawing(Binary) ->
    Lines = binary:split(Binary, <<"\n">>, [global]),
    lists:foldl(fun parse_drawing_line/2, #{}, lists:reverse(Lines)).

parse_drawing_line(Line, Stacks) ->
    parse_drawing_line(Line, Stacks, 1).

parse_drawing_line(<<" 1 ", _/binary>>, Stacks, _NextId) ->
    Stacks; % ignore the header-line
parse_drawing_line(<<"   ">>, Stacks, _NextId) ->
    Stacks;
parse_drawing_line(<<"[", Char:1/bytes, "]">>, Stacks, NextId) ->
    add_to_stack(Char, Stacks, NextId);
parse_drawing_line(<<"    ", Rest/binary>>, Stacks, NextId) ->
    parse_drawing_line(Rest, Stacks, NextId + 1);
parse_drawing_line(<<"[", Char:1/bytes, "] ", Rest/binary>>, Stacks, NextId) ->
    parse_drawing_line(Rest, add_to_stack(Char, Stacks, NextId), NextId + 1).

add_to_stack(Char, Stacks, Id) when is_map_key(Id, Stacks) ->
    Stack = maps:get(Id, Stacks),
    Stacks#{Id => [Char|Stack]};
add_to_stack(Char, Stacks, Id) ->
    Stacks#{Id => [Char]}.

parse_commands(Binary) ->
    Lines = binary:split(Binary, <<"\n">>, [global]),
    lists:map(fun parse_command_line/1, Lines).

parse_command_line(Line) ->
    io:format("Line=~p~n", [Line]),
    {match, [Count, From, To]} = re:run(Line, <<"move (\\d+) from (\\d+) to (\\d+)">>, [anchored, {capture, all_but_first, binary}]),
    {move, binary_to_integer(Count), binary_to_integer(From), binary_to_integer(To)}.

execute_command({move, 1, From, To}, Stacks) ->
    move_one(From, To, Stacks);
execute_command({move, N, From, To}, Stacks) ->
    Stacks1 = move_one(From, To, Stacks),
    execute_command({move, N-1, From, To}, Stacks1).

move_one(From, To, Stacks) ->
    io:format("Move from ~p to ~p~n", [From, To]),
    #{From := [Head|FromRest], To := ToStack} = Stacks,
    Stacks#{From => FromRest, To => [Head|ToStack]}.

read_tops(Stacks) ->
    read_tops(Stacks, 1, []).
read_tops(Stacks, N, Acc) when is_map_key(N, Stacks) ->
    #{N := [Top|_]} = Stacks,
    read_tops(Stacks, N+1, [Top|Acc]);
read_tops(_, _, Acc) ->
    lists:reverse(Acc).

execute_command_2({move, N, From, To}, Stacks) ->
    #{From := FromStack, To := ToStack} = Stacks,
    {Head, NewFrom} = lists:split(N, FromStack),
    Stacks#{From => NewFrom, To := Head ++ ToStack}.
