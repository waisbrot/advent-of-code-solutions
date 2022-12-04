-module(aoc_problem_4).
-export([solve/2]).

solve(Problem, Example) ->
    RawInput = aoc_input:split_by_lines(Problem, Example),
    Input = lists:map(fun parse_input/1, RawInput),
    %io:format("Input=~p~n", [Input]),
    SubsetCount = lists:foldl(fun (Elves, Count) -> Count + either_subset(Elves) end, 0, Input),
    io:format("Part 1: ~p~n", [SubsetCount]),
    OverlapCount = lists:foldl(fun (Elves, Count) -> Count + any_overlap(Elves) end, 0, Input),
    io:format("Part 2: ~p~n", [OverlapCount]).

parse_input(Line) ->
    Elves = binary:split(Line, <<",">>),
    lists:map(fun parse_input_elf/1, Elves).

parse_input_elf(E) -> 
    lists:map(fun erlang:binary_to_integer/1, binary:split(E, <<"-">>)).

either_subset([A, B]) ->
    case subset(A, B) of
        1 -> 1;
        0 -> subset(B, A)
    end.

subset([From1, To1], [From2, To2]) when From1 >= From2 andalso To1 =< To2 -> 1;
subset(_, _) -> 0.

any_overlap([A,B]) ->
    case any_overlap_helper(A, B) of
        1 -> 1;
        0 -> any_overlap_helper(B, A)
    end.

any_overlap_helper([From1, _To1], [From2, To2]) when From1 >= From2 andalso From1 =< To2 -> 1;
any_overlap_helper([_From1, To1], [From2, To2]) when To1 >= From2 andalso To1 =< To2 -> 1;
any_overlap_helper(_, _) -> 0.