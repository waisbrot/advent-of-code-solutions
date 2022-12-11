-module(aoc_problem_10).
-export([solve/2]).

solve(Problem, Example) ->
    Instructions = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Instructions]),
    aoc_timer:problem(2, fun solve_2/1, [Instructions]).

ingest(Problem, Example) ->
    Lines = aoc_input:split_by_lines(Problem, Example),
    lists:map(fun 
        (<<"noop">>) -> noop;
        (<<"addx ", Val/binary>>) -> {addx, binary_to_integer(Val)}
    end, Lines).

solve_1(Instructions) ->
    % io:format("Instructions=~p~n", [Instructions]),
    execute_and_accumulate(Instructions, 0, 1, [20, 60, 100, 140, 180, 220], 0).

execute_and_accumulate(_, _, _, [], Accum) ->
    Accum;
execute_and_accumulate([noop|Instructions], KeyCycle, X, [KeyCycle|Rest], Accum) ->
    Signal = KeyCycle * X,
    io:format("noop. Signal at cycle ~p: ~p (~p * ~p)~n", [KeyCycle, Signal, KeyCycle, X]),
    execute_and_accumulate(Instructions, KeyCycle+1, X, Rest, Accum + Signal);
execute_and_accumulate([{addx, N}|Instructions], Cycle, X, [KeyCycle|Rest], Accum) when Cycle =:= KeyCycle orelse Cycle+1 =:= KeyCycle ->
    Signal = KeyCycle * X,
    io:format("addx ~p. Signal at cycle ~p (~p): ~p (~p * ~p)~n", [N, KeyCycle, Cycle, Signal, KeyCycle, X]),
    execute_and_accumulate(Instructions, Cycle+2, X+N, Rest, Accum + Signal);
execute_and_accumulate([noop|Instructions], Cycle, X, KeyCycles, Accum) ->
    % io:format("~p: X=~p: noop~n", [Cycle, X]),
    execute_and_accumulate(Instructions, Cycle+1, X, KeyCycles, Accum);
execute_and_accumulate([{addx, N}|Instructions], Cycle, X, KeyCycles, Accum) ->
    % io:format("~p: X=~p: addx ~p~n", [Cycle, X, N]),
    execute_and_accumulate(Instructions, Cycle+2, X+N, KeyCycles, Accum).

solve_2(Instructions) ->
    ok.