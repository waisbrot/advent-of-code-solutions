-module(aoc_problem_1).
-export([solve/2]).

-spec solve(string, bool) -> any.
solve(Problem, Example) ->
    ElvesStrs = aoc_input:split_by_blanks(Problem, Example),
    %io:format("ElvesStr=~p~n", [ElvesStrs]),
    Elves = lists:map(fun parse_elf/1, ElvesStrs),
    ElfCals = lists:map(fun lists:sum/1, Elves),
    MaxElf = lists:max(ElfCals),
    io:format("Part 1: ~p~n", [MaxElf]),
    SortedElfCals = lists:reverse(lists:sort(ElfCals)),
    %io:format("SortedElfCals=~p~n", [SortedElfCals]),
    [E1, E2, E3 | _] = SortedElfCals,
    Top3 = lists:sum([E1, E2, E3]),
    io:format("Part 2: ~p~n", [Top3]).

parse_elf(ElfString) ->
    %io:format("ElfString=~p~n", [ElfString]),
    ElfParts = binary:split(ElfString, <<"\n">>, [global]),
    %io:format("ElfParts=~p~n", [ElfParts]),
    lists:map(fun erlang:binary_to_integer/1, ElfParts).