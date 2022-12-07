-module(aoc_timer).
-export([input/2, problem/3]).

input(Function, Args) ->
    {TimeInput, Input} = timer:tc(Function, Args),
    io:format("Read input: ~f seconds~n", [TimeInput * 10.0e-7]),
    Input.


problem(N, Function, Args) ->
    {Time, Solution} = timer:tc(Function, Args),
    io:format("Problem ~b: ~p in ~f seconds~n", [N, Solution, Time * 10.0e-7]),
    Solution.
