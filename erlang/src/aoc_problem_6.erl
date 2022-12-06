-module(aoc_problem_6).
-export([solve/2]).

solve(Problem, Example) ->
    {TimeInput, Binary} = timer:tc(fun aoc_input:simple_read/2, [Problem, Example]),
    io:format("Read input: ~p microseconds~n", [TimeInput]),
    {TimeFirst, Position} = timer:tc(fun first_marker/1, [Binary]),
    io:format("Problem 1: ~p in ~p microseconds~n", [Position, TimeFirst]),
    {TimeMessage, Position2} = timer:tc(fun first_message/1, [Binary]),
    io:format("Problem 2: ~p in ~p microseconds~n", [Position2, TimeMessage]).

first_marker(<<B1:1/bytes, B2:1/bytes, B3:1/bytes, B4:1/bytes, Rest/binary>>) ->
    first_marker(4, [B1, B2, B3, B4], 4, Rest).

first_marker(Target, [_ | BufTail] = Buffer, N, Stream) ->
    case length(sets:to_list(sets:from_list(Buffer))) of
        Target ->
            % solved
            N;
        _ ->
            <<NextByte:1/bytes, Rest/binary>> = Stream,
            Buffer2 = BufTail ++ [NextByte],
            first_marker(Target, Buffer2, N + 1, Rest)
    end.

first_message(<<B1:1/bytes, B2:1/bytes, B3:1/bytes, B4:1/bytes, B5:1/bytes, B6:1/bytes, B7:1/bytes, B8:1/bytes, B9:1/bytes, B10:1/bytes, B11:1/bytes, B12:1/bytes, B13:1/bytes, B14:1/bytes, Rest/binary>>) ->
        first_marker(14, [B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14], 14, Rest).
